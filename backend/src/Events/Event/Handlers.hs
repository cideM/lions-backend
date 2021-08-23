module Events.Event.Handlers
  ( getAll,
    get,
    getConfirmDelete,
    postDelete,
    getCreate,
    postCreate,
    postUpdate,
    getEdit,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Foldable (find)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Events.Attachments.Actions (Actions (..))
import qualified Events.Attachments.Actions as Attachments.Actions
import qualified Events.Attachments.Saved as Saved
import qualified Events.Attachments.Temporary as Temporary
import qualified Events.Event.Event as Event
import qualified Events.Event.Form as EventForm
import qualified Events.Event.Html as Event.Html
import qualified Events.Event.Id as Event
import Events.Reply.Reply (Reply (..))
import qualified Events.Reply.Reply as Reply
import GHC.Exts (sortWith)
import qualified Katip as K
import Layout (ActiveNavLink (..), LayoutStub (..), infoBox, success)
import Locale (german)
import Lucid
import qualified Network.Wai as Wai
import Network.Wai.Parse
  ( ParseRequestBodyOptions,
    defaultParseRequestBodyOptions,
    parseRequestBodyEx,
    setMaxRequestFileSize,
    tempFileBackEnd,
  )
import qualified User.Session
import Wai (paramsToMap)

getAll ::
  (MonadIO m, MonadReader env m, MonadThrow m, App.HasDb env) =>
  User.Session.Authenticated ->
  m LayoutStub
getAll auth = do
  events <- Event.getAll

  now <- liftIO $ Time.getCurrentTime

  let (past, upcoming) = partition ((>=) now . Event.eventDate . snd) events
      upcomingSorted = sortWith (Event.eventDate . snd) upcoming
      formatted = map (formatEventData now) (upcomingSorted ++ past)

  return . LayoutStub "Veranstaltungen" (Just Events) $ eventPreviewsHtml formatted
  where
    formatEventData now (eventid, event@Event.Event {..}) =
      let User.Session.Session {..} = User.Session.get' auth
          userReply = find ((==) sessionUserId . Reply.replyUserId) eventReplies
          isExpired = Event.Html.IsExpired (now >= eventDate)
       in (eventid, event, userReply, isExpired)

    -- Ideally there's zero markup in the handler files.
    eventPreviewsHtml ::
      [ ( Event.Id,
          Event.Event Saved.FileName,
          Maybe Reply,
          Event.Html.IsExpired
        )
      ] ->
      Html ()
    eventPreviewsHtml events =
      div_ [class_ "container"] $ do
        div_ [class_ "row row-cols-1 g-3"] $ do
          div_ [class_ "col"] $ do
            when (User.Session.isAdmin' auth) $
              a_ [class_ "mb-2 btn btn-sm btn-primary", href_ "/veranstaltungen/neu", role_ "button"] "Neue Veranstaltung"
            h1_ [class_ "h3 m-0"] "Veranstaltungen"
          div_ [class_ "col"] $ do
            div_
              [class_ "row row-cols-1 gy-4"]
              $ do
                div_ [class_ "col"] $ infoBox "Zum Öffnen einer Veranstaltung einfach auf den Titel klicken"
                (mapM_ (div_ [class_ "col"] . Event.Html.preview) events)

get ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Event.Id ->
  User.Session.Authenticated ->
  m (Maybe LayoutStub)
get eventid auth = do
  now <- liftIO $ Time.getCurrentTime

  let userIsAdmin = User.Session.isAdmin' auth
      User.Session.Session {..} = User.Session.get' auth

  Event.get eventid >>= \case
    Nothing -> return Nothing
    Just e@Event.Event {..} -> do
      let isExpired = Event.Html.IsExpired (now >= eventDate)
          ownReply = find ((==) sessionUserId . Reply.replyUserId) eventReplies
          isAdmin = Event.Html.ShowAdminTools userIsAdmin
          html = Event.Html.full isExpired isAdmin ownReply eventid e

      return . Just $ LayoutStub eventTitle (Just Events) html

getConfirmDelete ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m,
    MonadThrow m
  ) =>
  Event.Id ->
  User.Session.Admin ->
  m LayoutStub
getConfirmDelete eid@(Event.Id eventid) _ = do
  (Event.get eid) >>= \case
    Nothing -> throwString $ "delete event but no event for id: " <> show eventid
    Just event -> do
      return . LayoutStub "Veranstaltung Löschen" (Just Events) $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-danger mb-4", role_ "alert"] $
              toHtml ("Veranstaltung " <> Event.eventTitle event <> " wirklich löschen?")
            form_
              [ action_ . Text.pack $ "/veranstaltungen/" <> show eventid <> "/loeschen",
                method_ "post",
                class_ "d-flex justify-content-center"
              ]
              $ button_ [class_ "btn btn-primary", type_ "submit"] "Ja, Veranstaltung löschen!"

postDelete ::
  ( MonadIO m,
    App.HasDb env,
    App.HasEventStorage env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Event.Id ->
  User.Session.Admin ->
  m LayoutStub
postDelete eventid _ = do
  maybeEvent <- Event.get eventid
  case maybeEvent of
    Nothing -> throwString $ "delete event but no event for id: " <> show eventid
    Just event -> do
      Saved.removeAll eventid
      Event.delete eventid
      return $
        LayoutStub "Veranstaltung" (Just Events) $
          success $ "Veranstaltung " <> Event.eventTitle event <> " erfolgreich gelöscht"

getEdit ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m,
    MonadThrow m
  ) =>
  Event.Id ->
  User.Session.Admin ->
  m LayoutStub
getEdit eid@(Event.Id eventid) _ = do
  (Event.get eid) >>= \case
    Nothing -> throwString $ "edit event but no event for id: " <> show eventid
    Just Event.Event {..} ->
      let input =
            EventForm.FormInput
              eventTitle
              (renderDateForInput eventDate)
              eventLocation
              eventDescription
              eventFamilyAllowed
              (zip (map Saved.unFileName eventAttachments) (repeat True))
              []
       in return . LayoutStub "Veranstaltung Editieren" (Just Events) $
            div_ [class_ "container p-2"] $ do
              h1_ [class_ "h4 mb-3"] "Veranstaltung editieren"
              EventForm.render "Speichern" (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren") input EventForm.emptyState
  where
    renderDateForInput = Text.pack . Time.formatTime german "%d.%m.%Y %R"

getCreate :: (MonadIO m) => User.Session.Admin -> m LayoutStub
getCreate _ = do
  return . LayoutStub "Neue Veranstaltung" (Just Events) $
    div_ [class_ "container p-2"] $ do
      h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
      EventForm.render "Speichern" "/veranstaltungen/neu" EventForm.emptyForm EventForm.emptyState

fileUploadOpts :: ParseRequestBodyOptions
fileUploadOpts = setMaxRequestFileSize 100000000 defaultParseRequestBodyOptions

postCreate ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    K.KatipContext m,
    App.HasEventStorage env,
    App.HasSessionEncryptionKey env,
    App.HasInternalState env,
    App.HasDb env
  ) =>
  Wai.Request ->
  User.Session.Admin ->
  m LayoutStub
postCreate req _ = do
  internalState <- asks App.getInternalState
  -- There's no point in trying to catch an exception arising from a payload
  -- that's too large. The connection will be closed, so no more bytes are read
  -- by the server, and the browser will likely just show a connection reset
  -- browser error. Meaning the user will have no idea what's going on.
  -- Therefore the limit is set really high. If I want to enforce a lower limit
  -- but also show nice errors that means always reading the entire request
  -- payload and then doing a check for its size. For now I just won't enforce
  -- any file limit other than the hard 100MB.
  K.katipAddNamespace "postCreate" $ do
    body <- liftIO $ parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) req

    let params = paramsToMap $ fst body
        fromParams key = Map.findWithDefault "" key params

    (actions@Actions {..}, encryptedFileInfos) <- Attachments.Actions.make [] body

    -- Most of this code shows up in the postUpdate handler as well, at least
    -- in some form. Not sure if I want to further extract shared code.
    -- Dependencies are always a double-edged sword.
    -- makeActions includes all the logic for combining the various sources of input:
    --   - New files
    --   - Checkboxes from previous request
    --   - Files already uploaded and stored in DB
    --  The code here is then just to turn that state into UI
    let notSavedAndDeleteCheckbox =
          map
            (\(Temporary.Attachment {..}) -> (attachmentFileName, False))
            actionsDontUpload

        uploadCheckbox = zip (map Temporary.attachmentFileName actionsUpload) (repeat True)

        checkboxes = notSavedAndDeleteCheckbox ++ uploadCheckbox

        input =
          EventForm.FormInput
            (fromParams "eventTitleInput")
            (fromParams "eventDateInput")
            (fromParams "eventLocationInput")
            (fromParams "eventDescriptionInput")
            (isJust $ Map.lookup "eventFamAllowedInput" params)
            checkboxes
            encryptedFileInfos

    case EventForm.makeEvent input of
      Left state -> do
        return . LayoutStub "Neue Veranstaltung" (Just Events) $
          div_ [class_ "container p-2"] $ do
            h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
            EventForm.render "Speichern" "/veranstaltungen/neu" input state
      Right newEvent@Event.Event {..} -> do
        eid <- Event.save newEvent

        Attachments.Actions.apply eid actions

        return . LayoutStub "Neue Veranstaltung" (Just Events) $
          success $ "Neue Veranstaltung " <> eventTitle <> " erfolgreich erstellt!"

postUpdate ::
  ( MonadThrow m,
    MonadIO m,
    K.KatipContext m,
    App.HasEventStorage env,
    App.HasInternalState env,
    App.HasSessionEncryptionKey env,
    MonadReader env m,
    App.HasDb env
  ) =>
  Wai.Request ->
  Event.Id ->
  User.Session.Admin ->
  m LayoutStub
postUpdate req eid@(Event.Id eventid) _ = do
  internalState <- asks App.getInternalState
  body <- liftIO $ parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) req

  K.katipAddNamespace "postUpdate" $ do
    Event.get eid >>= \case
      Nothing -> throwString $ "edit event but no event for id: " <> show eventid
      Just Event.Event {..} -> do
        (actions@Actions {..}, encryptedFileInfos) <- Attachments.Actions.make eventAttachments body

        let params = paramsToMap $ fst body
            fromParams key = Map.findWithDefault "" key params

            savedButDeleteCheckbox = zip (map Saved.unFileName actionsDelete) (repeat False)

            keepCheckbox = zip (map Saved.unFileName actionsKeep) (repeat True)

            notSavedAndDeleteCheckbox =
              map
                (\(Temporary.Attachment {..}) -> (attachmentFileName, False))
                actionsDontUpload

            uploadCheckbox = zip (map Temporary.attachmentFileName actionsUpload) (repeat True)

            checkboxes =
              keepCheckbox ++ savedButDeleteCheckbox ++ notSavedAndDeleteCheckbox
                ++ uploadCheckbox

            input =
              EventForm.FormInput
                (fromParams "eventTitleInput")
                (fromParams "eventDateInput")
                (fromParams "eventLocationInput")
                (fromParams "eventDescriptionInput")
                (isJust $ Map.lookup "eventFamAllowedInput" params)
                checkboxes
                encryptedFileInfos

        case EventForm.makeEvent input of
          Left state ->
            return $
              LayoutStub "Veranstaltung Editieren" (Just Events) $
                div_ [class_ "container p-3 d-flex justify-content-center"] $
                  EventForm.render
                    "Speichern"
                    (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren")
                    input
                    state
          Right newEvent -> do
            Attachments.Actions.apply eid actions

            Event.update eid newEvent

            return $
              LayoutStub "Veranstaltung Editieren" (Just Events) $
                success $ "Veranstaltung " <> Event.eventTitle newEvent <> " erfolgreich editiert"
