module Server (run) where

import qualified App
import Control.Exception.Safe
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Default as Def
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified Env
import Events.API (EventID (..))
import qualified Events.API as EventsAPI
import qualified Feed.Message
import qualified Katip as K
import Layout (ActiveNavLink (..), LayoutStub (..), layout, warning)
import qualified Logging
import qualified Login.Login as Login
import Lucid
import Network.HTTP.Types (status200, status403, status404, status500)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import qualified Network.Wai.Middleware.Gzip as Gzip
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified Password.Change.Handlers
import qualified Password.Reset.Handlers
import qualified Request.Middleware as Request
import qualified Session.Middleware as Session
import Text.Read (readEither)
import qualified UnliftIO
import qualified User.Handler as User
import qualified User.Id as User
import qualified User.Session
import qualified Wai as Wai
import qualified WelcomeMessage
import Prelude hiding (id)

server ::
  ( K.KatipContext m,
    MonadIO m,
    MonadThrow m,
    App.HasRequestIdVaultKey env,
    App.HasEnvironment env,
    UnliftIO.MonadUnliftIO m,
    App.HasSessionEncryptionKey env,
    MonadCatch m,
    App.HasInternalState env,
    App.HasSessionDataVaultKey env,
    App.HasScryptSignerKey env,
    App.HasScryptSaltSeparator env,
    App.HasDb env,
    App.HasPort env,
    App.HasMail env,
    MonadReader env m
  ) =>
  Wai.ApplicationT m
server req send = do
  reqIdVaultKey <- asks App.getRequestIdVaultKey
  sessionDataVaultKey <- asks App.getSessionDataVaultKey

  let vault = Wai.vault req

      -- Create some helpers for doing things based on the user's auth status.
      authInfo = User.Session.fromVault sessionDataVaultKey vault

      -- This function let's visitors access a resource if:
      -- - They're an admin
      -- - They're accessing a resource associated with their own user ID
      adminOnlyOrOwn id next = do
        case User.Session.getAuth authInfo of
          Nothing -> send403
          Just auth -> do
            let User.Session.Session {..} = User.Session.get' auth
                isAdmin = User.Session.isAdmin authInfo
                isOwn = sessionUserId == id

            if isAdmin || isOwn
              then next (id, auth)
              else send403

      adminOnly' next = maybe send403 next (User.Session.getAdmin authInfo)

      authenticatedOnly' next = maybe send403 next (User.Session.getAuth authInfo)

      -- The layout changes depending on whether you're logged in or not, so
      -- we dependency inversion through partial application. Witness the
      -- simplicity!
      layout' = layout authInfo

      requestId = maybe "" (Text.pack . show) $ Vault.lookup reqIdVaultKey vault

      -- Some helpers related to rendering content. I could look into
      -- bringing back Snap or something similar so I don't need to
      -- reimplement these helpers in a crappy and bug ridden way.
      headers = [("Content-Type", "text/html; charset=UTF-8")]
      render code = send . Wai.responseLBS code headers . renderBS
      send200 = render status200
      send403 = render status403 . layout' Nothing . LayoutStub "Fehler" $ warning "Du hast keinen Zugriff auf diese Seite"
      send404 = render status404 . layout' Nothing . LayoutStub "Nicht gefunden" $ warning "Nicht Gefunden"

  K.katipAddContext (K.sl "request_id" requestId) $ do
    case Wai.pathInfo req of
      [] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ WelcomeMessage.showFeed >=> send200 . layout' (Just Welcome)
          _ -> send404
      ["veranstaltungen"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ EventsAPI.getAll >=> send200 . layout' (Just Events)
          _ -> send404
      ["veranstaltungen", "neu"] ->
        case Wai.requestMethod req of
          "GET" -> adminOnly' $ EventsAPI.getCreate >=> send200 . layout' (Just Events)
          "POST" ->
            adminOnly' $ EventsAPI.postCreate req >=> send200 . layout' (Just Events)
          _ -> send404
      ["veranstaltungen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Integer) ->
            case Wai.requestMethod req of
              "GET" ->
                authenticatedOnly' $
                  EventsAPI.get req (EventID parsed) >=> \case
                    Nothing -> send404
                    Just stub -> send200 $ layout' (Just Events) stub
              _ -> send404
      ["veranstaltungen", i, "antwort"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Integer) ->
            case Wai.requestMethod req of
              "POST" ->
                authenticatedOnly' $
                  EventsAPI.postUpdateEventReplies req send (EventID parsed)
              _ -> send404
      ["veranstaltungen", i, "loeschen"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Integer) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ EventsAPI.getConfirmDelete (EventID parsed) >=> send200 . layout' (Just Events)
              "POST" -> adminOnly' $ EventsAPI.postDelete (EventID parsed) >=> send200 . layout' (Just Events)
              _ -> send404
      ["veranstaltungen", i, "editieren"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Integer) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ EventsAPI.getEdit (EventID parsed) >=> send200 . layout' (Just Events)
              "POST" ->
                adminOnly' $
                  EventsAPI.postUpdate req (EventID parsed)
                    >=> send200 . layout' (Just Events)
              _ -> send404
      ["loeschen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = Feed.Message.Id parsed
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ WelcomeMessage.handleDeleteMessage msgId >=> send200 . layout' (Just Welcome)
                  "GET" -> adminOnly' $ WelcomeMessage.showDeleteConfirmation msgId >=> send200 . layout' (Just Welcome)
                  _ -> send404
      ["neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ WelcomeMessage.saveNewMessage req >=> send200 . layout' (Just Welcome)
          "GET" -> adminOnly' $ WelcomeMessage.showAddMessageForm >=> send200 . layout' (Just Welcome)
          _ -> send404
      ["editieren", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = Feed.Message.Id parsed
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ WelcomeMessage.handleEditMessage req msgId >=> send200 . layout' (Just Welcome)
                  "GET" -> adminOnly' $ WelcomeMessage.showMessageEditForm msgId >=> send200 . layout' (Just Welcome)
                  _ -> send404
      ["nutzer"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ User.viewListGet req >=> send200 . layout' (Just Members)
          _ -> send404
      ["nutzer", "neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ User.createPost req >=> send200 . layout' (Just Members)
          "GET" -> adminOnly' (User.createGet >=> send200 . layout' (Just Members))
          _ -> send404
      ["nutzer", int, "editieren"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for User.Id as int: " <> int
          Right (parsed :: Int) ->
            let userId = User.Id parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnlyOrOwn userId $
                    \(id, auth) ->
                      User.editGet id auth
                        >>= send200 . layout' (Just Profile)
                  "POST" -> adminOnlyOrOwn userId $
                    \(id, auth) ->
                      User.editPost req id auth
                        >>= send200 . layout' (Just Profile)
                  _ -> send404
      ["nutzer", int] ->
        case Wai.requestMethod req of
          "GET" ->
            case readEither (Text.unpack int) of
              Left _ -> throwString . Text.unpack $ "couldn't parse route param for User.Id as int: " <> int
              Right (parsed :: Int) ->
                authenticatedOnly' $
                  User.viewGet parsed >=> \case
                    Nothing -> send404
                    Just stub -> send200 $ layout' (Just Profile) stub
          _ -> send404
      ["nutzer", int, "loeschen"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for User.Id as int: " <> int
          Right (parsed :: Int) ->
            let userId = User.Id parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnly' $ User.deleteGet userId >=> send200 . layout' (Just Members)
                  "POST" -> adminOnly' $ User.deletePost userId >=> send200 . layout' (Just Members)
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
          "GET" -> Password.Change.Handlers.get req >>= send200 . layout' (Just Login)
          "POST" -> Password.Change.Handlers.post req >>= send200 . layout' (Just Login)
          _ -> send404
      ["passwort", "link"] ->
        case Wai.requestMethod req of
          "GET" -> Password.Reset.Handlers.get >>= send200 . layout' (Just Login)
          "POST" -> Password.Reset.Handlers.post req >>= send200 . layout' (Just Login)
          _ -> send404
      _ -> send404

run :: IO ()
run = do
  Env.withAppEnv $ \env@App.Env {..} -> do
    K.katipAddContext (K.sl "port" envPort) $ do
      K.logLocM K.InfoS "starting server"

      let defaultGzipSettings = (Def.def :: Gzip.GzipSettings)
          gzipMiddleware =
            Gzip.gzip $
              defaultGzipSettings
                { Gzip.gzipFiles = Gzip.GzipCompress,
                  Gzip.gzipCheckMime = Gzip.defaultCheckMime
                }

      let middlewares =
            (Wai.liftMiddleware gzipMiddleware)
              . (Wai.liftMiddleware $ staticPolicy (addBase "public"))
              . Request.middleware
              -- It's important that the attachments middleware comes after the session
              -- middleware, so that the event attachments are not accessible by the
              -- public.
              . Session.middleware
              . Logging.middleware
              . EventsAPI.attachmentsMiddleware

      let settings = setPort envPort $ setHost "0.0.0.0" defaultSettings
          app = middlewares server
          onError s e = do
            K.logLocM K.ErrorS (K.ls $ show e)
            s send500
          appWithErrorsHandled req send =
            let send' = liftIO . send
             in flip App.unApp env . handleAny (onError send') $ app req send'

      liftIO $ runSettings settings appWithErrorsHandled
  where
    send500 = do
      Wai.responseLBS status500 [("Content-Type", "text/html; charset=UTF-8")]
      . renderBS
      . layout User.Session.notAuthenticated Nothing
      . LayoutStub "Fehler"
      $ div_ [class_ "container p-3 d-flex justify-content-center"]
      $ div_ [class_ "row col-6"]
      $ do
        p_ [class_ "alert alert-secondary", role_ "alert"] "Es ist leider ein Fehler aufgetreten"
