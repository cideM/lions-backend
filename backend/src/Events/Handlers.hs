module Events.Handlers
  ( getAll,
    getCreate,
    get,
    getConfirmDelete,
    postDelete,
    postCreate,
    postUpdate,
    getEdit,
    postReply,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Resource (InternalState)
import Data.Foldable (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Events.AttachmentInfo as Events
import qualified Events.Attachments as A
import qualified Events.DB
import Events.FileActions (FileActions (..))
import qualified Events.Form as EventForm
import qualified Events.Full
import qualified Events.Preview
import qualified Events.Types as Events
import GHC.Exts (sortWith)
import qualified Katip as K
import Layout (ActiveNavLink (..), LayoutStub (..), infoBox, success)
import Locale (german)
import Lucid
import Network.HTTP.Types (status303)
import qualified Network.Wai as Wai
import Network.Wai.Parse
  ( ParseRequestBodyOptions,
    defaultParseRequestBodyOptions,
    parseRequestBodyEx,
    setMaxRequestFileSize,
    tempFileBackEnd,
  )
import qualified Session.Auth as Auth
import Text.Read (readEither)
import User.Types (UserId (..), UserProfile (..))
import qualified User.Types as User
import Wai (paramsToMap, parseParams)

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
  events <- toEventList <$> Events.DB.getAll
  return . LayoutStub "Veranstaltungen" (Just Events) $ eventPreviewsHtml events
  where
    -- Every event includes all replies but the view functions shouldn't have
    -- to do that logic, so we just extract it. This technically introduces
    -- redundancy. Maybe it would be better to just define a lens for that.
    -- TODO: Read the optics book and apply it
    getOwnReplyFromEvent (eventid, event@Events.Event {..}) =
      let User.Session {..} = Auth.get' auth
          reply = find ((==) sessionUserId . Events.replyUserId) eventReplies
       in (eventid, event, reply)

    toEventList = map getOwnReplyFromEvent . sortWith (Down . Events.eventDate . snd)

    -- Ideally there's zero markup in the handler files.
    -- TODO: If you're super bored with lots of time on your hands extract the
    -- markup
    eventPreviewsHtml :: [(Events.Id, Events.Event, Maybe Events.Reply)] -> Html ()
    eventPreviewsHtml events =
      let userIsAdmin = Auth.isAdmin' auth
       in page userIsAdmin (mapM_ (div_ [class_ "col"] . Events.Preview.render) events)

postReply ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  (UserId -> IO (Maybe UserProfile)) ->
  Wai.Request ->
  (Wai.Response -> m a) ->
  Events.Id ->
  Auth.Authenticated ->
  m a
postReply getUser req send eventid@(Events.Id eid) auth = do
  let User.Session {..} = Auth.get' auth

  UserProfile {userEmail = userEmail} <-
    (liftIO $ getUser sessionUserId) >>= \case
      Nothing -> throwString [i|"no user for userid from session #{sessionUserId}"|]
      Just v -> return v

  (coming, numberOfGuests) <- liftIO $ parseParams'

  -- This is the core branching in this handler, everything else is just
  -- necessary edge cases and parsing inputs. The "Nothing" case is for when
  -- the user doesn't want commit to either coming or not coming, which I model
  -- as having no reply. The "Just" case is then either yes or no, which
  -- doesn't matter. What matters is that I now store that repl.
  case coming of
    Nothing -> Events.DB.deleteReply eventid sessionUserId
    Just yesno ->
      Events.DB.upsertReply eventid $
        (Events.Reply yesno userEmail sessionUserId numberOfGuests)

  send $ Wai.responseLBS status303 [("Location", encodeUtf8 [i|/veranstaltungen/#{eid}|])] mempty
  where
    -- 10 lines of code that say two params are required
    parseParams' :: IO (Maybe Bool, Int)
    parseParams' = do
      params <- parseParams req

      let replyParam = Map.findWithDefault "" "reply" params
          numGuestsParam = Map.findWithDefault "" "numberOfGuests" params

      case parseComing replyParam of
        Left e -> throwString $ Text.unpack e
        Right coming -> case parseNumGuests numGuestsParam of
          Left e -> throwString $ Text.unpack e
          Right numGuests -> return (coming, numGuests)

    parseComing :: Text -> Either Text (Maybe Bool)
    parseComing "coming" = Right $ Just True
    parseComing "notcoming" = Right $ Just False
    parseComing "noreply" = Right Nothing
    parseComing s = Left [i|unknown coming value '#{s :: Text.Text}'|]

    parseNumGuests :: Text -> Either Text Int
    parseNumGuests "" = Right 0
    parseNumGuests s =
      case readEither (Text.unpack s) of
        Left e -> Left [i|couldn't parse '#{s}' as number: #{e}|]
        Right i' -> Right i'

get ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Events.Id ->
  Auth.Authenticated ->
  m (Maybe LayoutStub)
get eventid auth = do
  let userIsAdmin = Auth.isAdmin' auth
      User.Session {..} = Auth.get' auth

  Events.DB.get eventid >>= \case
    Nothing -> return Nothing
    Just e@Events.Event {..} -> do
      let ownReply = find ((==) sessionUserId . Events.replyUserId) eventReplies
      return . Just
        . LayoutStub
          eventTitle
          (Just Events)
        $ Events.Full.render (Events.Full.ShowAdminTools userIsAdmin) ownReply eventid e

getCreate :: (MonadIO m) => Auth.Admin -> m LayoutStub
getCreate _ = do
  return . LayoutStub "Neue Veranstaltung" (Just Events) $
    div_ [class_ "container p-2"] $ do
      h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
      EventForm.render "Speichern" "/veranstaltungen/neu" EventForm.emptyForm EventForm.emptyState

postDelete ::
  ( MonadIO m,
    App.HasDb env,
    App.HasEventStorage env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Events.Id ->
  Auth.Admin ->
  m LayoutStub
postDelete eventid _ = do
  maybeEvent <- Events.DB.get eventid
  case maybeEvent of
    Nothing -> return . LayoutStub "Fehler" (Just Events) $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just event -> do
      A.removeAll eventid
      Events.DB.delete eventid
      return $
        LayoutStub "Veranstaltung" (Just Events) $
          success $ "Veranstaltung " <> Events.eventTitle event <> " erfolgreich gelöscht"

getConfirmDelete ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m,
    MonadThrow m
  ) =>
  Events.Id ->
  Auth.Admin ->
  m LayoutStub
getConfirmDelete eid@(Events.Id eventid) _ = do
  -- TODO: Duplicated
  (Events.DB.get eid) >>= \case
    Nothing -> throwString $ "delete event but no event for id: " <> show eventid
    Just event -> do
      return . LayoutStub "Veranstaltung Löschen" (Just Events) $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-danger mb-4", role_ "alert"] $
              toHtml ("Veranstaltung " <> Events.eventTitle event <> " wirklich löschen?")
            form_
              [ action_ . Text.pack $ "/veranstaltungen/" <> show eventid <> "/loeschen",
                method_ "post",
                class_ "d-flex justify-content-center"
              ]
              $ button_ [class_ "btn btn-primary", type_ "submit"] "Ja, Veranstaltung löschen!"

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

    (actions@FileActions {..}, encryptedFileInfos) <- A.makeFileActions [] body

    -- Most of this code shows up in the postUpdate handler as well, at least
    -- in some form. Not sure if I want to further extract shared code.
    -- Dependencies are always a double-edged sword.
    -- makeFileActions includes all the logic for combining the various sources of input:
    --   - New files
    --   - Checkboxes from previous request
    --   - Files already uploaded and stored in DB
    --  The code here is then just to turn that state into UI
    let notSavedAndDeleteCheckbox =
          map
            (\(Events.AttachmentInfo {..}) -> (attachmentInfoFileName, False))
            fileActionsDontUpload

        uploadCheckbox = zip (map Events.attachmentInfoFileName fileActionsUpload) (repeat True)

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
      Right newEvent@Events.Create {..} -> do
        eid <- Events.DB.save newEvent

        A.processFileActions eid actions

        return . LayoutStub "Neue Veranstaltung" (Just Events) $
          success $ "Neue Veranstaltung " <> createTitle <> " erfolgreich erstellt!"

getEdit ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m,
    MonadThrow m
  ) =>
  Events.Id ->
  Auth.Admin ->
  m LayoutStub
getEdit eid@(Events.Id eventid) _ = do
  (Events.DB.get eid) >>= \case
    Nothing -> throwString $ "edit event but no event for id: " <> show eventid
    Just Events.Event {..} ->
      let input =
            EventForm.FormInput
              eventTitle
              (renderDateForInput eventDate)
              eventLocation
              eventDescription
              eventFamilyAllowed
              (zip (map Events.attachmentFileName eventAttachments) (repeat True))
              []
       in return . LayoutStub "Veranstaltung Editieren" (Just Events) $
            div_ [class_ "container p-2"] $ do
              h1_ [class_ "h4 mb-3"] "Veranstaltung editieren"
              EventForm.render "Speichern" (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren") input EventForm.emptyState
  where
    renderDateForInput = Text.pack . Time.formatTime german "%d.%m.%Y %R"

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
  Events.Id ->
  Auth.Admin ->
  m LayoutStub
postUpdate internalState req eid@(Events.Id eventid) _ = do
  body <- liftIO $ parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) req

  K.katipAddNamespace "postUpdate" $ do
    Events.DB.get eid >>= \case
      Nothing -> throwString $ "edit event but no event for id: " <> show eventid
      Just Events.Event {..} -> do
        (actions@FileActions {..}, encryptedFileInfos) <- A.makeFileActions eventAttachments body

        let params = paramsToMap $ fst body
            fromParams key = Map.findWithDefault "" key params

            savedButDeleteCheckbox = zip (map Events.attachmentFileName fileActionsDelete) (repeat False)

            keepCheckbox = zip (map Events.attachmentFileName fileActionsKeep) (repeat True)

            notSavedAndDeleteCheckbox =
              map
                (\(Events.AttachmentInfo {..}) -> (attachmentInfoFileName, False))
                fileActionsDontUpload

            uploadCheckbox = zip (map Events.attachmentInfoFileName fileActionsUpload) (repeat True)

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
          Right event@Events.Create {..} -> do
            A.processFileActions eid actions

            Events.DB.update eid event

            return $
              LayoutStub "Veranstaltung Editieren" (Just Events) $
                success $ "Veranstaltung " <> createTitle <> " erfolgreich editiert"
