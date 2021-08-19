module Lib (server, main) where

import qualified App
import Control.Exception.Safe
import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified Env
import qualified Events.Attachments.Middleware as AttachmentsMiddleware
import qualified Events.Event.Handlers as Event.Handlers
import qualified Events.Event.Id as Event
import qualified Events.Reply.Handlers as Reply.Handlers
import qualified Katip as K
import Layout (LayoutStub (..), layout, warning)
import qualified Login.Login as Login
import Lucid
import Network.HTTP.Types (status200, status403, status404, status500)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified Password.Change.Handlers
import qualified Password.Reset.Handlers
import qualified Request.Middleware as Request
import qualified Session.Middleware as Session
import Text.Read (readEither)
import qualified UnliftIO
import qualified User.Handler as User.User
import qualified User.Id as User
import qualified User.Session
import qualified User.Session as User
import qualified Userlist
import qualified Userprofile
import qualified Wai.Class as Wai
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
      -- TODO: Test this
      adminOnlyOrOwn id next =
        maybe send403 (next . (id,)) $ do
          auth <- User.Session.getAuth authInfo
          unless (User.Session.isAdmin authInfo) Nothing
          let User.Session {..} = User.Session.get' auth
          unless (sessionUserId /= id) Nothing
          pure auth

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
      send403 = render status403 . layout' . LayoutStub "Fehler" Nothing $ warning "Du hast keinen Zugriff auf diese Seite"
      send404 = render status404 . layout' . LayoutStub "Nicht gefunden" Nothing $ warning "Nicht Gefunden"

  K.katipAddContext (K.sl "request_id" requestId) $ do
    K.logLocM K.InfoS "request received"

    case Wai.pathInfo req of
      [] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ WelcomeMessage.showFeed >=> send200 . layout'
          _ -> send404
      ["veranstaltungen"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ Event.Handlers.getAll >=> send200 . layout'
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", "neu"] ->
        case Wai.requestMethod req of
          "GET" -> adminOnly' $ Event.Handlers.getCreate >=> send200 . layout'
          "POST" ->
            adminOnly' $ Event.Handlers.postCreate req >=> send200 . layout'
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" ->
                authenticatedOnly' $
                  Event.Handlers.get (Event.Id parsed) >=> \case
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
                  Reply.Handlers.post req send (Event.Id parsed)
              _ -> send404
      ["veranstaltungen", i, "loeschen"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ Event.Handlers.getConfirmDelete (Event.Id parsed) >=> send200 . layout'
              "POST" -> adminOnly' $ Event.Handlers.postDelete (Event.Id parsed) >=> send200 . layout'
              _ -> send404
      ["veranstaltungen", i, "editieren"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ Event.Handlers.getEdit (Event.Id parsed) >=> send200 . layout'
              "POST" ->
                adminOnly' $
                  Event.Handlers.postUpdate req (Event.Id parsed)
                    >=> send200 . layout'
              _ -> send404
      ["loeschen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = WelcomeMsgId parsed
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ WelcomeMessage.handleDeleteMessage msgId >=> send200 . layout'
                  "GET" -> adminOnly' $ WelcomeMessage.showDeleteConfirmation msgId >=> send200 . layout'
                  _ -> send404
      ["neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ WelcomeMessage.saveNewMessage req >=> send200 . layout'
          "GET" -> adminOnly' $ WelcomeMessage.showAddMessageForm >=> send200 . layout'
          _ -> send404
      ["editieren", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = WelcomeMsgId parsed
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ WelcomeMessage.handleEditMessage req msgId >=> send200 . layout'
                  "GET" -> adminOnly' $ WelcomeMessage.showMessageEditForm msgId >=> send200 . layout'
                  _ -> send404
      ["nutzer"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ Userlist.get req >=> send200 . layout'
          _ -> send404
      ["nutzer", "neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ User.User.createPost req >=> send200 . layout'
          "GET" -> adminOnly' (User.User.createGet >=> send200 . layout')
          _ -> send404
      ["nutzer", int, "editieren"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for User.Id as int: " <> int
          Right (parsed :: Int) ->
            let userId = User.Id parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnlyOrOwn userId $
                    \(id, auth) ->
                      User.User.editGet id auth
                        >>= send200 . layout'
                  "POST" -> adminOnlyOrOwn userId $
                    \(id, auth) ->
                      User.User.editPost req id auth
                        >>= send200 . layout'
                  _ -> send404
      ["nutzer", int] ->
        case Wai.requestMethod req of
          "GET" ->
            case readEither (Text.unpack int) of
              Left _ -> throwString . Text.unpack $ "couldn't parse route param for User.Id as int: " <> int
              Right (parsed :: Int) ->
                authenticatedOnly' $
                  Userprofile.get parsed >=> \case
                    Nothing -> send404
                    Just stub -> send200 $ layout' stub
          _ -> send404
      ["nutzer", int, "loeschen"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for User.Id as int: " <> int
          Right (parsed :: Int) ->
            let userId = User.Id parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnly' $ User.User.deleteGet userId >=> send200 . layout'
                  "POST" -> adminOnly' $ User.User.deletePost userId >=> send200 . layout'
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
  Env.withAppEnv $ \env@App.Env {..} -> do
    K.katipAddContext (K.sl "port" envPort) $ do
      K.logLocM K.InfoS "starting server"

      let assetMiddleware :: (UnliftIO.MonadUnliftIO m) => Wai.MiddlewareT m
          assetMiddleware = Wai.liftMiddleware $ staticPolicy (addBase "public")
          -- Must come after sessionMiddleware because these files shouldn't be public
          storageStaticMiddleware :: (UnliftIO.MonadUnliftIO m) => Wai.MiddlewareT m
          storageStaticMiddleware = Wai.liftMiddleware $ staticPolicy (addBase envEventAttachmentStorageDir)
          allMiddlewares =
            assetMiddleware
              . (Wai.liftMiddleware logStdout)
              . Request.middleware
              . Session.middleware
              . AttachmentsMiddleware.middleware envEventAttachmentStorageDir
              . storageStaticMiddleware
          appWithMiddlewares = allMiddlewares server

      let settings = setPort envPort $ setHost "localhost" defaultSettings
      liftIO . runSettings settings $
        ( \r s ->
            let send = liftIO . s
             in flip App.unApp env
                  . handleAny (\_ -> send500 send)
                  $ appWithMiddlewares r send
        )
  where
    send500 send = do
      send
        . Wai.responseLBS status500 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout User.Session.notAuthenticated
        . LayoutStub "Fehler" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Es ist leider ein Fehler aufgetreten"
