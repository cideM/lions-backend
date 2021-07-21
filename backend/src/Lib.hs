module Lib (server, main) where

import qualified App
import Control.Exception.Safe
import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import qualified DB
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import qualified Events.Attachments
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
import qualified Password.Change.Handlers
import qualified Password.Reset.Handlers
import qualified Password.Reset.Mail as Mail
import qualified Request.Middleware as Request
import qualified Session.Auth as Session
import qualified Session.Middleware as Session
import qualified System.Directory
import System.Environment (getEnv)
import Text.Read (readEither)
import qualified UnliftIO
import qualified User.DB
import User.Types (UserId (..))
import qualified User.Types as User
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
    App.HasEnvironment env,
    UnliftIO.MonadUnliftIO m,
    App.HasEventStorage env,
    App.HasSessionEncryptionKey env,
    App.HasScryptSignerKey env,
    App.HasScryptSaltSeparator env,
    App.HasDb env,
    App.HasPort env,
    App.HasMail env,
    MonadReader env m
  ) =>
  SQLite.Connection ->
  Session.VaultKey ->
  InternalState ->
  Wai.ApplicationT m
server dbConn sessionDataVaultKey internalState req send = do
  reqIdVaultKey <- asks App.getRequestIdVaultKey

  let vault = Wai.vault req

      -- Create some helpers for doing things based on the user's auth status.
      authInfo = Session.fromVault sessionDataVaultKey vault

      -- This function let's visitors access a resource if:
      -- - They're an admin
      -- - They're accessing a resource associated with their own user ID
      -- TODO: Test this
      adminOnlyOrOwn id next =
        maybe send403 (next . (id,)) $ do
          auth <- Session.getAuth authInfo
          unless (Session.isAdmin authInfo) Nothing
          let User.Session {..} = Session.get' auth
          unless (sessionUserId /= id) Nothing
          pure auth

      adminOnly' next = maybe send403 next (Session.getAdmin authInfo)

      authenticatedOnly' next = maybe send403 next (Session.getAuth authInfo)

      -- The layout changes depending on whether you're logged in or not, so
      -- we dependency inversion through partial application. Witness the
      -- simplicity!
      layout' = layout authInfo

      requestId = maybe "" (Text.pack . show) $ Vault.lookup reqIdVaultKey vault

      -- TODO: Remove
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
          "GET" -> authenticatedOnly' $ Events.Handlers.getAll >=> send200 . layout'
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", "neu"] ->
        case Wai.requestMethod req of
          "GET" -> adminOnly' $ Events.Handlers.getCreate >=> send200 . layout'
          "POST" ->
            adminOnly' $ Events.Handlers.postCreate internalState req >=> send200 . layout'
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" ->
                authenticatedOnly' $
                  Events.Handlers.get (Events.Id parsed) >=> \case
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
                  Events.Handlers.postReply getUser req send (Events.Id parsed)
              _ -> send404
      ["veranstaltungen", i, "loeschen"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ Events.Handlers.getConfirmDelete (Events.Id parsed) >=> send200 . layout'
              "POST" -> adminOnly' $ Events.Handlers.postDelete (Events.Id parsed) >=> send200 . layout'
              _ -> send404
      ["veranstaltungen", i, "editieren"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ Events.Handlers.getEdit (Events.Id parsed) >=> send200 . layout'
              "POST" ->
                adminOnly' $
                  Events.Handlers.postUpdate internalState req (Events.Id parsed)
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
          "POST" -> Login.postLogin req send
          "GET" -> Login.getLogin authInfo >>= send200
          _ -> send404
      ["logout"] ->
        case Wai.requestMethod req of
          "POST" -> Login.postLogout (Vault.lookup sessionDataVaultKey) req send
          _ -> send404
      ["passwort", "aendern"] ->
        case Wai.requestMethod req of
          "GET" -> Password.Change.Handlers.get req >>= send200 . layout'
          "POST" -> Password.Change.Handlers.post req >>= send200 . layout'
          _ -> send404
      ["passwort", "link"] ->
        case Wai.requestMethod req of
          "GET" -> Password.Reset.Handlers.get >>= send200 . layout'
          "POST" -> Password.Reset.Handlers.post req >>= send200 . layout'
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
                Logging.withKatip
                  K.DebugS
                  "main"
                  (K.Environment . Text.pack $ show appEnv)
                  $ do
                    ctx <- K.getKatipContext
                    ns <- K.getKatipNamespace
                    logEnv <- K.getLogEnv

                    let port = (3000 :: Int)

                        env =
                          App.Env
                            { envDatabaseConnection = conn,
                              envEnvironment = appEnv,
                              envScryptSignerKey = signerKey,
                              envMail = Mail.send awsEnv,
                              envScryptSaltSeparator = saltSep,
                              envPort = port,
                              envEventAttachmentStorageDir = storageDir,
                              envSessionDataVaultKey = sessionDataVaultKey,
                              envRequestIdVaultKey = requestIdVaultKey,
                              envSessionEncryptionKey = sessionKey,
                              envLogNamespace = ns,
                              envLogContext = ctx,
                              envLogEnv = logEnv
                            }

                        app' =
                          server
                            conn
                            sessionDataVaultKey
                            internalState

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
                              . Request.middleware
                              . Session.middleware
                              . Events.Attachments.middleware storageDir
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
        . layout Session.notAuthenticated
        . LayoutStub "Fehler" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Es ist leider ein Fehler aufgetreten"
