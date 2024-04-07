{-# LANGUAGE TemplateHaskell #-}

module Server (app) where

import qualified Activities.API as Activities
import qualified App
import Control.Exception.Safe
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Default as Def
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import Events.API (EventID (..))
import qualified Events.API as EventsAPI
import qualified Feed.API as FeedAPI
import qualified Feed.Message
import qualified Feed.Middleware as FeedAttachmentsMiddleware
import Katip
import Layout (ActiveNavLink (..), LayoutStub (..), layout, warning)
import qualified Logging
import qualified Login.Login as Login
import Lucid
import Network.HTTP.Types (status200, status403, status404, status405, status500)
import qualified Network.Wai as Wai
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
import qualified User.Session as Session
import qualified Wai as Wai
import Prelude hiding (id)

app ::
  ( KatipContext m,
    MonadIO m,
    MonadThrow m,
    App.HasRequestIdVaultKey env,
    UnliftIO.MonadUnliftIO m,
    App.HasSessionEncryptionKey env,
    MonadCatch m,
    App.HasSessionDataVaultKey env,
    App.HasScryptSignerKey env,
    App.HasScryptSaltSeparator env,
    App.HasDb env,
    App.HasAWS env,
    MonadReader env m
  ) =>
  Wai.ApplicationT m
app request send =
  let defaultGzipSettings = (Def.def :: Gzip.GzipSettings)
      gzipMiddleware =
        Gzip.gzip $
          defaultGzipSettings
            { Gzip.gzipFiles = Gzip.GzipCompress,
              Gzip.gzipCheckMime = Gzip.defaultCheckMime
            }

      middlewares =
        (Wai.liftMiddleware gzipMiddleware)
          . (Wai.liftMiddleware $ staticPolicy (addBase "public"))
          . Request.middleware
          -- It's important that the attachments middleware comes after the session
          -- middleware, so that the event attachments are not accessible by the
          -- public.
          . Session.middleware
          . EventsAPI.attachmentsMiddleware
          . FeedAttachmentsMiddleware.middleware
          . Logging.middleware
   in handleAny (\e -> onError e request send) $ middlewares routes request send

routes ::
  ( KatipContext m,
    MonadIO m,
    MonadThrow m,
    App.HasRequestIdVaultKey env,
    UnliftIO.MonadUnliftIO m,
    App.HasSessionEncryptionKey env,
    MonadCatch m,
    App.HasSessionDataVaultKey env,
    App.HasScryptSignerKey env,
    App.HasScryptSaltSeparator env,
    App.HasDb env,
    App.HasAWS env,
    MonadReader env m
  ) =>
  Wai.ApplicationT m
routes req send = do
  reqIdVaultKey <- asks App.getRequestIdVaultKey
  sessionDataVaultKey <- asks App.getSessionDataVaultKey

  let vault = Wai.vault req

      -- Create some helpers for doing things based on the user's auth status.
      authInfo = Session.fromVault sessionDataVaultKey vault

      -- This function let's visitors access a resource if:
      -- - They're an admin
      -- - They're accessing a resource associated with their own user ID
      adminOnlyOrOwn id next = do
        case Session.getAuth authInfo of
          Just auth
            | (Session.sessionUserId $ Session.get' auth) == id -> next (id, auth)
            | Session.isAdmin authInfo -> next (id, auth)
            | otherwise -> send403
          _ -> send403

      adminOnly' next = maybe send403 next (Session.getAdmin authInfo)

      authenticatedOnly' next = maybe send403 next (Session.getAuth authInfo)

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

  katipAddContext (sl "request_id" requestId) $ do
    case Wai.pathInfo req of
      [] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ FeedAPI.showFeed >=> send200 . layout' (Just Welcome)
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
      ["activities"] -> do
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ Activities.getAll >=> send200 . layout' (Just Activities)
          _ -> render status405 . layout' Nothing . LayoutStub "Fehler" $ mempty
      ["activities", "create"] -> do
        case Wai.requestMethod req of
          "GET" -> adminOnly' $ Activities.getCreate >=> send200 . layout' (Just Activities)
          "POST" -> adminOnly' $ Activities.postCreate req >=> send200 . layout' (Just Activities)
          _ -> render status405 . layout' Nothing . LayoutStub "Fehler" $ mempty
      ["activities", _, "times", timeId, "delete"] -> do
        case readEither (Text.unpack timeId) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for time ID as int: " <> timeId
          Right (parsedTimeId :: Int) ->
            case Wai.requestMethod req of
              "GET" -> authenticatedOnly' $ Activities.getConfirmDeleteTime parsedTimeId >=> send200 . layout' (Just Activities)
              "POST" -> authenticatedOnly' $ Activities.postDeleteTime parsedTimeId >=> send200 . layout' (Just Activities)
              _ -> render status405 . layout' Nothing . LayoutStub "Fehler" $ mempty
      ["activities", i, "edit"] -> do
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for activity ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ Activities.getEdit parsed >=> send200 . layout' (Just Activities)
              "POST" -> adminOnly' $ Activities.postEdit parsed req >=> send200 . layout' (Just Activities)
              _ -> render status405 . layout' Nothing . LayoutStub "Fehler" $ mempty
      ["activities", i, "delete"] -> do
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for activity ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ Activities.getConfirmDelete parsed >=> send200 . layout' (Just Activities)
              "POST" -> adminOnly' $ Activities.postDelete parsed >=> send200 . layout' (Just Activities)
              _ -> render status405 . layout' Nothing . LayoutStub "Fehler" $ mempty
      ["activities", i] -> do
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for activity ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> authenticatedOnly' $ Activities.get parsed req >=> send200 . layout' (Just Activities)
              "POST" -> authenticatedOnly' $ Activities.postAddTime parsed req >=> send200 . layout' (Just Activities)
              _ -> render status405 . layout' Nothing . LayoutStub "Fehler" $ mempty
      ["loeschen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = Feed.Message.Id parsed
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ FeedAPI.handleDeleteMessage msgId >=> send200 . layout' (Just Welcome)
                  "GET" -> adminOnly' $ FeedAPI.showDeleteConfirmation msgId >=> send200 . layout' (Just Welcome)
                  _ -> send404
      ["neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ FeedAPI.saveNewMessage req >=> send200 . layout' (Just Welcome)
          "GET" -> adminOnly' $ FeedAPI.showAddMessageForm >=> send200 . layout' (Just Welcome)
          _ -> send404
      ["editieren", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = Feed.Message.Id parsed
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ FeedAPI.handleEditMessage req msgId >=> send200 . layout' (Just Welcome)
                  "GET" -> adminOnly' $ FeedAPI.showMessageEditForm msgId >=> send200 . layout' (Just Welcome)
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

onError :: (KatipContext m, MonadIO m) => SomeException -> Wai.ApplicationT m
onError err _ send = do
  $(logTM) ErrorS (ls $ show err)

  send
    . Wai.responseLBS status500 [("Content-Type", "text/html; charset=UTF-8")]
    . renderBS
    . layout Session.notAuthenticated Nothing
    . LayoutStub "Fehler"
    $ div_ [class_ "container p-3 d-flex justify-content-center"]
    $ div_ [class_ "row col-6"]
    $ do
      p_ [class_ "alert alert-secondary", role_ "alert"] "Es ist leider ein Fehler aufgetreten"
