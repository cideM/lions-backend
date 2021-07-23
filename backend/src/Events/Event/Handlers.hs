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
import Data.Maybe (isJust)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import Data.Foldable (find)
import qualified Katip as K
import Control.Monad.Trans.Resource (InternalState)
import qualified Network.Wai as Wai
import qualified Data.Map.Strict as Map
import Events.Attachments.Actions (Actions (..))
import Data.Ord (Down (..))
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Events.Attachments.Actions as Attachments.Actions
import qualified Events.Attachments.Saved as Saved
import qualified Events.Attachments.Temporary as Temporary
import qualified Events.Event.Html as Event.Html
import qualified Events.Event.Form as EventForm
import qualified Events.Event.Event as Event
import qualified Events.Event.Id as Event
import Events.Reply.Reply (Reply (..))
import qualified Events.Reply.Reply as Reply
import GHC.Exts (sortWith)
import Layout (ActiveNavLink (..), LayoutStub (..), infoBox, success)
import Locale (german)
import Lucid
import Network.Wai.Parse
  ( ParseRequestBodyOptions,
    defaultParseRequestBodyOptions,
    parseRequestBodyEx,
    setMaxRequestFileSize,
    tempFileBackEnd,
  )
import qualified Session.Auth as Auth
import qualified User.Types as User
import Wai (paramsToMap)

-- TODO: Extract
page :: Bool -> Html () -> Html ()
page userIsAdmin content = do
  div_ [class_ "container"] $ do
    when userIsAdmin $
      a_ [class_ "mb-1 mb-3 btn btn-sm btn-primary", href_ "/veranstaltungen/neu", role_ "button"] "Neue Veranstaltung"
    h1_ [class_ "h3 mb-4"] "Veranstaltungen"
    div_ [class_ "my-3"] $ infoBox "Zum Öffnen einer Veranstaltung einfach auf den Titel klicken"
    div_ [class_ "row row-cols-1 row-cols-lg-2 gy-4 gx-lg-4"] content

getAll ::
  (MonadIO m, MonadReader env m, MonadThrow m, App.HasDb env) =>
  Auth.Authenticated ->
  m LayoutStub
getAll auth = do
  events <- toEventList <$> Event.getAll
  return . LayoutStub "Veranstaltungen" (Just Events) $ eventPreviewsHtml events
  where
    -- Every event includes all replies but the view functions shouldn't have
    -- to do that logic, so we just extract it. This technically introduces
    -- redundancy. Maybe it would be better to just define a lens for that.
    -- TODO: Read the optics book and apply it
    getOwnReplyFromEvent (eventid, event@Event.Event {..}) =
      let User.Session {..} = Auth.get' auth
          reply = find ((==) sessionUserId . Reply.replyUserId) eventReplies
       in (eventid, event, reply)

    toEventList = map getOwnReplyFromEvent . sortWith (Down . Event.eventDate . snd)

    -- Ideally there's zero markup in the handler files.
    -- TODO: If you're super bored with lots of time on your hands extract the
    -- markup
    eventPreviewsHtml :: [(Event.Id, Event.Event Saved.FileName, Maybe Reply)] -> Html ()
    eventPreviewsHtml events =
      let userIsAdmin = Auth.isAdmin' auth
       in page userIsAdmin (mapM_ (div_ [class_ "col"] . Event.Html.preview) events)

get ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Event.Id ->
  Auth.Authenticated ->
  m (Maybe LayoutStub)
get eventid auth = do
  let userIsAdmin = Auth.isAdmin' auth
      User.Session {..} = Auth.get' auth

  Event.get eventid >>= \case
    Nothing -> return Nothing
    Just e@Event.Event {..} -> do
      let ownReply = find ((==) sessionUserId . Reply.replyUserId) eventReplies
      return . Just
        . LayoutStub
          eventTitle
          (Just Events)
        $ Event.Html.full (Event.Html.ShowAdminTools userIsAdmin) ownReply eventid e

getConfirmDelete ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m,
    MonadThrow m
  ) =>
  Event.Id ->
  Auth.Admin ->
  m LayoutStub
getConfirmDelete eid@(Event.Id eventid) _ = do
  -- TODO: Duplicated
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
  Auth.Admin ->
  m LayoutStub
postDelete eventid _ = do
  maybeEvent <- Event.get eventid
  case maybeEvent of
    Nothing -> return . LayoutStub "Fehler" (Just Events) $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
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
  Auth.Admin ->
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

getCreate :: (MonadIO m) => Auth.Admin -> m LayoutStub
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
    App.HasDb env
  ) =>
  InternalState ->
  Wai.Request ->
  Auth.Admin ->
  m LayoutStub
postCreate internalState req _ = do
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
    App.HasSessionEncryptionKey env,
    MonadReader env m,
    App.HasDb env
  ) =>
  InternalState ->
  Wai.Request ->
  Event.Id ->
  Auth.Admin ->
  m LayoutStub
postUpdate internalState req eid@(Event.Id eventid) _ = do
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
