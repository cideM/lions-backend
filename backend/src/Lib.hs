module Lib (server, main) where

import qualified App
import Control.Exception.Safe
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import qualified DB
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import qualified Events.DB
import qualified Events.Handlers
import qualified Events.Types as Events
import qualified Katip as K
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
import qualified Request.Middleware
import Scrypt (verifyPassword)
import qualified Session
import qualified Session.Types as Session
import qualified Session.Middleware as Session
import qualified System.Directory
import System.Environment (getEnv)
import Text.Read (readEither)
import qualified UnliftIO
import qualified User.DB
import User.Types (UserId (..))
import qualified User.User
import qualified Userlist
import qualified Userprofile
import qualified Wai.Class as Wai
import qualified Web.ClientSession as ClientSession
import WelcomeMessage (WelcomeMsgId (..))
import qualified WelcomeMessage
import Prelude hiding (id)

server ::
  ( K.KatipContext m,
    MonadIO m,
    MonadThrow m,
    App.HasRequestIdVaultKey env,
    MonadReader env m
  ) =>
  SQLite.Connection ->
  ClientSession.Key ->
  AWS.Env ->
  Int ->
  App.Environment ->
  Session.SessionDataVaultKey ->
  BS.ByteString -> -- Project's base64_signer_key
  BS.ByteString -> -- Project's base64_salt_separator
  InternalState ->
  FilePath ->
  Wai.ApplicationT m
server
  dbConn
  sessionKey
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
    reqIdVaultKey <- asks App.getRequestIdVaultKey

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
        requestId = maybe "" (Text.pack . show) $ Vault.lookup reqIdVaultKey vault
        -- log' msg = Logging.log logger ([Interpolate.i|"request_id: #{(requestId :: Text.Text)} message: #{(msg :: Text.Text)}"|] :: Text.Text)

        -- TODO: The resetHost should probably be passed in as an argument from the outside
        resetHost =
          if appEnv == App.Production
            then "https://www.lions-achern.de"
            else Text.pack $ "http://localhost:" <> show port

        -- Dependency inversion through partial application. Ideally handlers
        -- don't ever require any DB or AWS dependencies. I still test handlers
        -- with integration tests because interactions with the DB are one of
        -- my primary sources of bugs. But it helps me focus on the clean.
        -- TODO: Make this more ergonomic maybe by bundling stuff in a record
        Events.DB.EventDb {..} = Events.DB.newEventDb dbConn
        sendMail' = SendEmail.sendMail awsEnv resetHost
        saveAttachment' = Events.Handlers.saveAttachment storageDir
        removeAllAttachments' = Events.Handlers.removeAllAttachments storageDir
        removeAttachment' = Events.Handlers.removeAttachment storageDir
        clientEncrypt = ClientSession.encryptIO sessionKey
        clientDecrypt = ClientSession.decrypt sessionKey
        tryLogin' = Session.tryLogin dbConn (verifyPassword signerKey saltSep) clientEncrypt
        getUser = User.DB.getUser dbConn
        -- Some helpers related to rendering content. I could look into
        -- bringing back Snap or something similar so I don't need to
        -- reimplement these helpers in a crappy and bug ridden way.
        headers = [("Content-Type", "text/html; charset=UTF-8")]
        render code = send . Wai.responseLBS code headers . renderBS
        send200 = render status200
        send403 = render status403 . layout' . LayoutStub "Fehler" Nothing $ warning "Du hast keinen Zugriff auf diese Seite"
        send404 = render status404 . layout' . LayoutStub "Nicht gefunden" Nothing $ warning "Nicht Gefunden"

    K.katipAddContext (K.sl "request_id" requestId) $ do
      K.logLocM K.InfoS "request received"

      case Wai.pathInfo req of
        [] ->
          case Wai.requestMethod req of
            "GET" -> authenticatedOnly' $ WelcomeMessage.showFeed dbConn >=> send200 . layout'
            _ -> send404
        ["veranstaltungen"] ->
          case Wai.requestMethod req of
            "GET" -> authenticatedOnly' $ Events.Handlers.showAllEvents eventDbAll >=> send200 . layout'
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
                  eventDbCreate
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
                    Events.Handlers.showEvent eventDbGet (Events.Id parsed) >=> \case
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
                    Events.Handlers.replyToEvent getUser eventDbDeleteReply eventDbUpsertReply req send (Events.Id parsed)
                _ -> send404
        ["veranstaltungen", i, "loeschen"] ->
          case readEither (Text.unpack i) of
            Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
            Right (parsed :: Int) ->
              case Wai.requestMethod req of
                "GET" -> adminOnly' $ Events.Handlers.showDeleteEventConfirmation eventDbGet (Events.Id parsed) >=> send200 . layout'
                "POST" -> adminOnly' $ Events.Handlers.handleDeleteEvent eventDbGet eventDbDelete removeAllAttachments' (Events.Id parsed) >=> send200 . layout'
                _ -> send404
        ["veranstaltungen", i, "editieren"] ->
          case readEither (Text.unpack i) of
            Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
            Right (parsed :: Int) ->
              case Wai.requestMethod req of
                "GET" -> adminOnly' $ Events.Handlers.showEditEventForm eventDbGet (Events.Id parsed) >=> send200 . layout'
                "POST" ->
                  adminOnly' $
                    Events.Handlers.handleUpdateEvent
                      eventDbUpdate
                      eventDbGet
                      clientEncrypt
                      clientDecrypt
                      removeAttachment'
                      saveAttachment'
                      internalState
                      req
                      (Events.Id parsed)
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
            "POST" -> Login.login tryLogin' appEnv req send
            "GET" -> Login.showLoginForm routeData >>= send200
            _ -> send404
        ["logout"] ->
          case Wai.requestMethod req of
            "POST" -> Login.logout (Session.deleteSessionsForUser dbConn) (Vault.lookup sessionDataVaultKey) appEnv req send
            _ -> send404
        ["passwort", "aendern"] ->
          case Wai.requestMethod req of
            "GET" -> PasswordReset.showChangePwForm req >>= send200 . layout'
            "POST" -> PasswordReset.handleChangePw dbConn req >>= send200 . layout'
            _ -> send404
        ["passwort", "link"] ->
          case Wai.requestMethod req of
            "GET" -> PasswordReset.showResetForm >>= send200 . layout'
            "POST" -> PasswordReset.handleReset dbConn req sendMail' >>= send200 . layout'
            _ -> send404
        _ -> send404

main :: IO ()
main = do
  sqlitePath <- getEnv "LIONS_SQLITE_PATH"
  appEnv <- getEnv "LIONS_ENV" >>= App.parseEnv
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
            ( \internalState -> do
                Logging.withKatip $ do
                  ctx <- K.getKatipContext
                  ns <- K.getKatipNamespace
                  logEnv <- K.getLogEnv

                  let env =
                        App.Env
                          { envDatabaseConnection = conn,
                            envEnvironment = appEnv,
                            envSessionKeyFile = sessionKeyFile,
                            envAwsAccessKey = aKey,
                            envAwsSecretAccessKey = sKey,
                            envScryptSignerKey = signerKey,
                            envScryptSaltSeparator = saltSep,
                            envEventAttachmentStorageDir = storageDir,
                            envSessionDataVaultKey = sessionDataVaultKey,
                            envRequestIdVaultKey = requestIdVaultKey,
                            envSessionEncryptionKey = sessionKey,
                            envLogNamespace = ns,
                            envLogContext = ctx,
                            envLogEnv = logEnv
                          }

                  let port = (3000 :: Int)
                      app' =
                        server
                          conn
                          sessionKey
                          awsEnv
                          port
                          appEnv
                          sessionDataVaultKey
                          signerKey
                          saltSep
                          internalState
                          storageDir

                  K.katipAddContext (K.sl "port" port) $ do
                    K.logLocM K.InfoS "starting server"

                    let assetMiddleware :: (UnliftIO.MonadUnliftIO m) => Wai.MiddlewareT m
                        assetMiddleware = Wai.liftMiddleware $ staticPolicy (addBase "public")
                        -- Must come after sessionMiddleware because these files shouldn't be public
                        storageStaticMiddleware :: (UnliftIO.MonadUnliftIO m) => Wai.MiddlewareT m
                        storageStaticMiddleware = Wai.liftMiddleware $ staticPolicy (addBase storageDir)
                        allMiddlewares =
                          assetMiddleware
                            . (Wai.liftMiddleware logStdout)
                            . Request.Middleware.middleware
                            . Session.middleware
                            . (Wai.liftMiddleware (staticPolicy (addBase storageDir)))
                            . storageStaticMiddleware
                        appWithMiddlewares = allMiddlewares app'

                    let settings = setPort port $ setHost "localhost" defaultSettings
                    liftIO . runSettings settings $
                      ( \r s ->
                          let send = liftIO . s
                           in flip App.unApp env
                                . handleAny (\_ -> send500 send)
                                $ appWithMiddlewares r send
                      )
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
