module Lib (server, main) where

import Control.Exception.Safe
import Control.Monad ((>=>))
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import qualified DB
import qualified Data.ByteString as BS
import qualified Data.String.Interpolate as Interpolate
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Env (Environment (..), parseEnv)
import qualified Events.DB
import Events.Domain (EventId (..))
import qualified Events.Handlers
import Layout (LayoutStub (..), layout, warning)
import qualified Logging
import qualified Login.Login as Login
import Lucid
import qualified Network.AWS as AWS
import Network.HTTP.Types (status200, status403, status404, status500)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified PasswordReset.PasswordReset as PasswordReset
import qualified PasswordReset.SendEmail as SendEmail
import Scrypt (verifyPassword)
import Session (SessionDataVaultKey)
import qualified Session
import qualified System.Directory
import System.Environment (getEnv)
import Text.Read (readEither)
import qualified User.DB
import User.Types (UserId (..))
import qualified User.User
import qualified Userlist
import qualified Userprofile
import qualified Web.ClientSession as ClientSession
import WelcomeMessage (WelcomeMsgId (..))
import qualified WelcomeMessage
import Prelude hiding (id)

server ::
  Logging.TimedFastLogger ->
  SQLite.Connection ->
  ClientSession.Key ->
  RequestIdVaultKey ->
  AWS.Env ->
  Int ->
  Environment ->
  SessionDataVaultKey ->
  BS.ByteString -> -- Project's base64_signer_key
  BS.ByteString -> -- Project's base64_salt_separator
  InternalState ->
  FilePath ->
  Wai.Request ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  IO Wai.ResponseReceived
server
  logger
  dbConn
  sessionKey
  requestIdVaultKey
  awsEnv
  port
  appEnv
  sessionDataVaultKey
  signerKey
  saltSep
  internalState
  storageDir
  req
  send = do
    let vault = Wai.vault req

        -- Create some helpers for doing things based on the user's auth status.
        routeData = Session.getAuthFromVault sessionDataVaultKey vault
        adminOnlyOrOwn id next = case routeData of
          Session.IsAuthenticated auth@(Session.IsAdmin _) -> next (id, auth)
          Session.IsAuthenticated auth@(Session.IsUser Session.UserSession {Session.userSessionUserId = userId}) ->
            if userId == id then next (id, auth) else send403
          _ -> send403
        adminOnly' next = case routeData of
          Session.IsAuthenticated (Session.IsAdmin auth) -> next auth
          _ -> send403
        authenticatedOnly' next = case routeData of
          Session.IsAuthenticated auth -> next auth
          _ -> send403

        -- The layout changes depending on whether you're logged in or not, so
        -- we dependency inversion through partial application. Witness the
        -- simplicity!
        layout' = layout routeData

        -- Because every logger in the Haskell ecosystem insists on forcing
        -- stupid Monads on me, I'm creating an ad-hoc, shoddily implemented,
        -- 50% structured logger.
        requestId = maybe "" (Text.pack . show) $ Vault.lookup requestIdVaultKey vault
        log' msg = Logging.log logger ([Interpolate.i|"request_id: #{(requestId :: Text.Text)} message: #{(msg :: Text.Text)}"|] :: Text.Text)

        -- TODO: The resetHost should probably be passed in as an argument from the outside
        resetHost =
          if appEnv == Production
            then "https://www.lions-achern.de"
            else Text.pack $ "http://localhost:" <> show port

        -- Dependency inversion through partial application. Ideally handlers
        -- don't ever require any DB or AWS dependencies. I still test handlers
        -- with integration tests because interactions with the DB are one of
        -- my primary sources of bugs. But it helps me focus on the clean.
        -- TODO: Make this more ergonomic maybe by bundling stuff in a record
        sendMail' = SendEmail.sendMail awsEnv resetHost
        saveAttachment' = Events.Handlers.saveAttachment storageDir
        removeAllAttachments' = Events.Handlers.removeAllAttachments storageDir
        removeAttachment' = Events.Handlers.removeAttachment storageDir
        clientEncrypt = ClientSession.encryptIO sessionKey
        clientDecrypt = ClientSession.decrypt sessionKey
        tryLogin' = Session.tryLogin dbConn (verifyPassword signerKey saltSep) clientEncrypt
        getAllEvents = Events.DB.getAll dbConn
        getUser = User.DB.getUser dbConn
        deleteReply = Events.DB.deleteReply dbConn
        upsertReply = Events.DB.upsertReply dbConn
        getEvent = Events.DB.getEvent dbConn
        deleteEvent = Events.DB.deleteEvent dbConn
        updateEvent = Events.DB.updateEvent dbConn

        -- Some helpers related to rendering content. I could look into
        -- bringing back Snap or something similar so I don't need to
        -- reimplement these helpers in a crappy and bug ridden way.
        headers = [("Content-Type", "text/html; charset=UTF-8")]
        render code = send . Wai.responseLBS code headers . renderBS
        send200 = render status200
        send403 = render status403 . layout' . LayoutStub "Fehler" Nothing $ warning "Du hast keinen Zugriff auf diese Seite"
        send404 = render status404 . layout' . LayoutStub "Nicht gefunden" Nothing $ warning "Nicht Gefunden"

    -- Now the actual routing starts. We get the paths and pattern match on them.
    case Wai.pathInfo req of
      [] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ WelcomeMessage.showFeed dbConn >=> send200 . layout'
          _ -> send404
      ["veranstaltungen"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ Events.Handlers.showAllEvents getAllEvents >=> send200 . layout'
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", "neu"] ->
        case Wai.requestMethod req of
          "GET" -> adminOnly' $ Events.Handlers.showCreateEvent >=> send200 . layout'
          "POST" ->
            adminOnly' $
              Events.Handlers.handleCreateEvent
                clientEncrypt
                clientDecrypt
                (Events.DB.createEvent dbConn)
                internalState
                saveAttachment'
                req
                >=> send200 . layout'
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" ->
                authenticatedOnly' $
                  Events.Handlers.showEvent getEvent (EventId parsed) >=> \case
                    Nothing -> send404
                    Just stub -> send200 $ layout' stub
              _ -> send404
      ["veranstaltungen", i, "antwort"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "POST" ->
                authenticatedOnly' $
                  Events.Handlers.replyToEvent getUser deleteReply upsertReply req send (EventId parsed)
              _ -> send404
      ["veranstaltungen", i, "loeschen"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ Events.Handlers.showDeleteEventConfirmation getEvent (EventId parsed) >=> send200 . layout'
              "POST" -> adminOnly' $ Events.Handlers.handleDeleteEvent getEvent deleteEvent removeAllAttachments' (EventId parsed) >=> send200 . layout'
              _ -> send404
      ["veranstaltungen", i, "editieren"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ Events.Handlers.showEditEventForm getEvent (EventId parsed) >=> send200 . layout'
              "POST" ->
                adminOnly' $
                  Events.Handlers.handleUpdateEvent
                    updateEvent
                    getEvent
                    clientEncrypt
                    clientDecrypt
                    removeAttachment'
                    saveAttachment'
                    internalState
                    req
                    (EventId parsed)
                    >=> send200 . layout'
              _ -> send404
      ["loeschen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = WelcomeMsgId parsed
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ WelcomeMessage.handleDeleteMessage dbConn msgId >=> send200 . layout'
                  "GET" -> adminOnly' $ WelcomeMessage.showDeleteConfirmation dbConn msgId >=> send200 . layout'
                  _ -> send404
      ["neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ WelcomeMessage.saveNewMessage dbConn req >=> send200 . layout'
          "GET" -> adminOnly' $ WelcomeMessage.showAddMessageForm >=> send200 . layout'
          _ -> send404
      ["editieren", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = WelcomeMsgId parsed
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ WelcomeMessage.handleEditMessage dbConn req msgId >=> send200 . layout'
                  "GET" -> adminOnly' $ WelcomeMessage.showMessageEditForm dbConn msgId >=> send200 . layout'
                  _ -> send404
      ["nutzer"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ Userlist.get dbConn req >=> send200 . layout'
          _ -> send404
      ["nutzer", "neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ User.User.createPost dbConn req >=> send200 . layout'
          "GET" -> adminOnly' (User.User.createGet >=> send200 . layout')
          _ -> send404
      ["nutzer", int, "editieren"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
          Right (parsed :: Int) ->
            let userId = UserId parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnlyOrOwn userId $
                    \(id, auth) ->
                      User.User.editGet dbConn id auth
                        >>= send200 . layout'
                  "POST" -> adminOnlyOrOwn userId $
                    \(id, auth) ->
                      User.User.editPost dbConn req id auth
                        >>= send200 . layout'
                  _ -> send404
      ["nutzer", int] ->
        case Wai.requestMethod req of
          "GET" ->
            case readEither (Text.unpack int) of
              Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
              Right (parsed :: Int) ->
                authenticatedOnly' $
                  Userprofile.get dbConn parsed >=> \case
                    Nothing -> send404
                    Just stub -> send200 $ layout' stub
          _ -> send404
      ["nutzer", int, "loeschen"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
          Right (parsed :: Int) ->
            let userId = UserId parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnly' $ User.User.deleteGet dbConn userId >=> send200 . layout'
                  "POST" -> adminOnly' $ User.User.deletePost dbConn userId >=> send200 . layout'
                  _ -> send404
      ["login"] ->
        case Wai.requestMethod req of
          "POST" -> Login.login log' tryLogin' appEnv req send
          "GET" -> Login.showLoginForm routeData >>= send200
          _ -> send404
      ["logout"] ->
        case Wai.requestMethod req of
          "POST" -> Login.logout (Session.deleteSessionsForUser dbConn) (Vault.lookup sessionDataVaultKey) appEnv req send
          _ -> send404
      ["passwort", "aendern"] ->
        case Wai.requestMethod req of
          "GET" -> PasswordReset.showChangePwForm req >>= send200 . layout'
          "POST" -> PasswordReset.handleChangePw log' dbConn req >>= send200 . layout'
          _ -> send404
      ["passwort", "link"] ->
        case Wai.requestMethod req of
          "GET" -> PasswordReset.showResetForm >>= send200 . layout'
          "POST" -> PasswordReset.handleReset dbConn req sendMail' >>= send200 . layout'
          _ -> send404
      _ -> send404

type RequestIdVaultKey = Vault.Key UUID

addRequestId :: RequestIdVaultKey -> Wai.Application -> Wai.Application
addRequestId requestIdVaultKey next req send = do
  uuid <- nextRandom
  let vault' = Vault.insert requestIdVaultKey uuid (Wai.vault req)
      req' = req {Wai.vault = vault'}
  next req' send

main :: IO ()
main = do
  sqlitePath <- getEnv "LIONS_SQLITE_PATH"
  appEnv <- getEnv "LIONS_ENV" >>= parseEnv
  sessionKeyFile <- getEnv "LIONS_SESSION_KEY_FILE"
  mailAwsAccessKey <- getEnv "LIONS_AWS_SES_ACCESS_KEY"
  mailAwsSecretAccessKey <- getEnv "LIONS_AWS_SES_SECRET_ACCESS_KEY"
  signerKey <- encodeUtf8 . Text.pack <$> getEnv "LIONS_SCRYPT_SIGNER_KEY"
  saltSep <- encodeUtf8 . Text.pack <$> getEnv "LIONS_SCRYPT_SALT_SEP"
  storageDir <- getEnv "LIONS_STORAGE_DIR"

  System.Directory.createDirectoryIfMissing True storageDir

  sessionKey <- ClientSession.getKey sessionKeyFile
  sessionDataVaultKey <- Vault.newKey
  requestIdVaultKey <- Vault.newKey

  DB.withConnection
    sqlitePath
    ( \conn -> do
        let aKey = AWS.AccessKey (encodeUtf8 (Text.pack mailAwsAccessKey))
            sKey = AWS.SecretKey (encodeUtf8 (Text.pack mailAwsSecretAccessKey))
        awsEnv <- AWS.newEnv (AWS.FromKeys aKey sKey)

        runResourceT $
          withInternalState
            ( \internalState ->
                Logging.withLogger $ \logger -> do
                  let port = (3000 :: Int)
                      app' =
                        server
                          logger
                          conn
                          sessionKey
                          requestIdVaultKey
                          awsEnv
                          port
                          appEnv
                          sessionDataVaultKey
                          signerKey
                          saltSep
                          internalState
                          storageDir

                  let requestIdMiddleware = addRequestId requestIdVaultKey
                      sessionMiddleware = Session.middleware (Logging.log logger) sessionDataVaultKey conn sessionKey
                      assetMiddleware = staticPolicy (addBase "public")
                      -- Must come after sessionMiddleware because these files shouldn't be public
                      storageStaticMiddleware = staticPolicy (addBase storageDir)
                      allMiddlewares = assetMiddleware . requestIdMiddleware . sessionMiddleware . storageStaticMiddleware
                      errorHandler send err = do
                        Logging.log logger $ show err
                        send500 send
                      finalApp req send =
                        handleAny (errorHandler send) $ allMiddlewares app' req send
                      warpServer = runSettings . setPort port $ setHost "localhost" defaultSettings

                  warpServer $ logStdout finalApp
            )
    )
  where
    send500 send = do
      send
        . Wai.responseLBS status500 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout Session.IsNotAuthenticated
        . LayoutStub "Fehler" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Es ist leider ein Fehler aufgetreten"
