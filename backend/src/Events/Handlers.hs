module Events.Handlers
  ( showAllEvents,
    showCreateEvent,
    showEvent,
    showDeleteEventConfirmation,
    handleDeleteEvent,
    handleCreateEvent,
    uploadAttachmentToS3,
    handleUpdateEvent,
    showEditEventForm,
    replyToEvent,
  )
where

import Control.Exception.Safe
import Control.Monad (when)
import Control.Monad.Trans.Resource (InternalState)
import qualified Data.ByteString as BS
import Data.Foldable (find)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Events.DB
  ( deleteEvent,
    deleteReply,
    getEvent,
    updateEvent,
    upsertReply,
  )
import Events.Domain (Event (..), EventCreate (..), EventId (..), Reply (..))
import qualified Events.EventForm as EventForm
import qualified Events.Preview
import qualified Events.SingleEvent
import GHC.Exts (sortWith)
import Layout (ActiveNavLink (..), LayoutStub (..), success)
import Locale (german)
import Lucid
import Network.AWS.Prelude (RqBody (..), toHashed)
import qualified Network.AWS.S3 as S3
import Network.HTTP.Types (status303)
import qualified Network.Wai as Wai
import Network.Wai.Parse
  ( File,
    FileInfo (..),
    Param,
    ParseRequestBodyOptions,
    defaultParseRequestBodyOptions,
    parseRequestBodyEx,
    setMaxRequestFileSize,
    tempFileBackEnd,
  )
import qualified Session
import Text.Read (readEither)
import User.DB (getUser)
import User.Types (UserId (..), UserProfile (..))
import Wai (paramsToMap, parseParams)

toEventList :: UserId -> M.Map EventId Event -> [(EventId, Event, Maybe Reply)]
toEventList userid = map getOwnReplyFromEvent . sortEvents . M.toList
  where
    sortEvents = sortWith (Down . eventDate . snd)
    getOwnReplyFromEvent (eventid, e) =
      let rs = eventReplies e
          reply = find ((==) userid . replyUserId) rs
       in (eventid, e, reply)

-- This displays each event as a little container that you can click to get to
-- the big event page.
eventPreviewsHtml :: Bool -> [(EventId, Event, Maybe Reply)] -> Html ()
eventPreviewsHtml userIsAdmin events =
  div_ [class_ "container"] $ do
    when userIsAdmin $
      a_ [class_ "mb-1 mb-2 btn btn-sm btn-primary", href_ "/veranstaltungen/neu", role_ "button"] "Neue Veranstaltung"
    h1_ [class_ "h3 mb-3"] "Veranstaltungen"
    div_ [class_ "row row-cols-1 row-cols-lg-2 gy-4 gx-lg-4"] $
      mapM_ (div_ [class_ "col"] . Events.Preview.render) events

showAllEvents :: IO (M.Map EventId Event) -> Session.Authenticated -> IO LayoutStub
showAllEvents getAllEvents auth = do
  let userIsAdmin = Session.isUserAdmin auth
      Session.UserSession {..} = Session.getSessionFromAuth auth
  events <- toEventList userSessionUserId <$> getAllEvents
  return . LayoutStub "Veranstaltungen" (Just Events) $ eventPreviewsHtml userIsAdmin events

replyToEvent ::
  SQLite.Connection ->
  Wai.Request ->
  (Wai.Response -> IO a) ->
  EventId ->
  Session.Authenticated ->
  IO a
replyToEvent conn req send eventid@(EventId i) auth = do
  let (_, Session.UserSession {..}) = case auth of
        Session.IsAdmin (Session.AdminUser session) -> (True, session)
        Session.IsUser session -> (False, session)
  UserProfile {..} <-
    getUser conn userSessionUserId >>= \case
      Nothing -> throwString $ "no user for userid from session, here's the user id: " <> show userSessionUserId
      Just v -> return v
  params <- parseParams req
  coming <-
    maybe (throwString "no 'reply' param") parseComing $
      Map.lookup "reply" params
  numberOfGuests <-
    maybe (throwString "no 'numberOfGuests' param") parseNumGuests $
      Map.lookup "numberOfGuests" params
  case coming of
    Nothing -> deleteReply conn eventid userSessionUserId
    Just yesno -> upsertReply conn eventid (Reply yesno userEmail userSessionUserId numberOfGuests)
  send $ Wai.responseLBS status303 [("Location", encodeUtf8 $ "/veranstaltungen/" <> Text.pack (show i))] mempty
  where
    parseComing "coming" = return $ Just True
    parseComing "notcoming" = return $ Just False
    parseComing "noreply" = return Nothing
    parseComing s = throwString . Text.unpack $ "unknown 'coming' value: " <> s

    parseNumGuests "" = return 0
    parseNumGuests s =
      case readEither (Text.unpack s) of
        Left e -> throwString . Text.unpack $ "couldn't parse '" <> s <> "' number of guests as number: " <> Text.pack (show e)
        Right i' -> return i'

showEvent ::
  SQLite.Connection ->
  EventId ->
  Session.Authenticated ->
  IO (Maybe LayoutStub)
showEvent conn eventid auth = do
  let (userIsAdmin, Session.UserSession {..}) = case auth of
        Session.IsAdmin (Session.AdminUser session) -> (True, session)
        Session.IsUser session -> (False, session)
  maybeevent <- getEvent conn eventid
  case maybeevent of
    Nothing -> return Nothing
    Just e@Event {..} -> do
      let ownReply = find ((==) userSessionUserId . replyUserId) eventReplies
      return . Just
        . LayoutStub
          eventTitle
          (Just Events)
        $ Events.SingleEvent.render (Events.SingleEvent.ShowAdminTools userIsAdmin) ownReply eventid e

showCreateEvent :: Session.AdminUser -> IO LayoutStub
showCreateEvent _ = do
  return . LayoutStub "Neue Veranstaltung" (Just Events) $
    div_ [class_ "container p-2"] $ do
      h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
      EventForm.render "Speichern" "/veranstaltungen/neu" EventForm.emptyForm EventForm.emptyState

handleDeleteEvent ::
  SQLite.Connection ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
handleDeleteEvent conn eventid _ = do
  maybeEvent <- getEvent conn eventid
  case maybeEvent of
    Nothing -> return . LayoutStub "Fehler" (Just Events) $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just event -> do
      deleteEvent conn eventid
      return $
        LayoutStub "Veranstaltung" (Just Events) $
          success $ "Veranstaltung " <> eventTitle event <> " erfolgreich gelöscht"

showDeleteEventConfirmation ::
  SQLite.Connection ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
showDeleteEventConfirmation conn eid@(EventId eventid) _ = do
  -- TODO: Duplicated
  getEvent conn eid >>= \case
    Nothing -> throwString $ "delete event but no event for id: " <> show eventid
    Just event -> do
      return . LayoutStub "Veranstaltung Löschen" (Just Events) $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-danger mb-4", role_ "alert"] $
              toHtml ("Veranstaltung " <> eventTitle event <> " wirklich löschen?")
            form_
              [ action_ . Text.pack $ "/veranstaltungen/" <> show eventid <> "/loeschen",
                method_ "post",
                class_ "d-flex justify-content-center"
              ]
              $ button_ [class_ "btn btn-primary", type_ "submit"] "Ja, Veranstaltung löschen!"

uploadAttachmentToS3 :: FilePath -> IO S3.PutObject
uploadAttachmentToS3 filePath = do
  contents <- BS.readFile filePath
  let key = S3.ObjectKey $ Text.pack filePath
  return $ S3.putObject "lions-achern-event-attachments" key $ Hashed $ toHashed contents

-- | File size 100MB
fileUploadOpts :: ParseRequestBodyOptions
fileUploadOpts = setMaxRequestFileSize 100000000 defaultParseRequestBodyOptions

type FileName = Text.Text

type FileChecked = (FileName, Bool)

parseBody :: ([Param], [File y]) -> [FileChecked]
parseBody body =
  -- All files newly added with this particular request
  let newFileNames = filter ("\"\"" /=) . map (decodeUtf8 . fileName . snd) $ snd body
      -- Unchecked checkboxes are not sent with the form, so we keep track of
      -- them through hidden input fields
      previousFilenames = map (decodeUtf8 . snd) . filter ((==) "allFiles" . fst) $ fst body
      allFileNames = newFileNames ++ previousFilenames
      -- checked checkboxes from current request
      checked = map (decodeUtf8 . snd) . filter ((==) "newFileCheckbox" . fst) $ fst body
      -- For each file generate a checkbox and optionally mark it was checked.
      checkboxes = map (\name -> (name, isJust $ find (name ==) checked)) allFileNames
   in checkboxes

handleCreateEvent ::
  (EventCreate -> IO ()) ->
  InternalState ->
  (FilePath -> IO S3.PutObjectResponse) ->
  Wai.Request ->
  Session.AdminUser ->
  IO LayoutStub
handleCreateEvent createEvent internalState uploadFile req _ = do
  -- There's no point in trying to catch an exception arising from a payload
  -- that's too large. The connection will be closed, so no more bytes are read
  -- by the server, and the browser will likely just show a connection reset
  -- browser error. Meaning the user will have no idea what's going on.
  -- Therefore the limit is set really high. If I want to enforce a lower limit
  -- but also show nice errors that means always reading the entire request
  -- payload and then doing a check for its size. For now I just won't enforce
  -- any file limit other than the hard 100MB.
  body <- parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) req
  -- ( [ ("eventTitleInput", "test"),
  --     ("eventLocationInput", "dawd"),
  --     ("eventDateInput", "19.07.2021 19:00"),
  --     ("eventDescriptionInput", "dwdaw")
  --   ],
  --   [ ( "eventAttachmentsInput",
  --       FileInfo
  --         { fileName = "bluetooth_adapter_invoice.pdf",
  --           fileContentType = "application/pdf",
  --           fileContent = "/var/folders/k5/1rl8klmn4sd910pxy672sznr0000gp/T/webenc25795-0.buf"
  --         }
  --     )
  --   ]
  --   )
  let params = paramsToMap $ fst body
      checkboxes = parseBody body
  let input =
        EventForm.FormInput
          (Map.findWithDefault "" "eventTitleInput" params)
          (Map.findWithDefault "" "eventDateInput" params)
          (Map.findWithDefault "" "eventLocationInput" params)
          (Map.findWithDefault "" "eventDescriptionInput" params)
          (isJust $ Map.lookup "eventFamAllowedInput" params)
          checkboxes
  case EventForm.makeEvent input of
    Left state -> do
      return . LayoutStub "Neue Veranstaltung" (Just Events) $
        div_ [class_ "container p-2"] $ do
          h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
          EventForm.render "Speichern" "/veranstaltungen/neu" input state
    Right newEvent@EventCreate {..} -> do
      createEvent newEvent
      return . LayoutStub "Neue Veranstaltung" (Just Events) $
        success $ "Neue Veranstaltung " <> eventCreateTitle <> " erfolgreich erstellt!"

showEditEventForm ::
  SQLite.Connection ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
showEditEventForm conn eid@(EventId eventid) _ = do
  getEvent conn eid >>= \case
    Nothing -> throwString $ "edit event but no event for id: " <> show eventid
    Just Event {..} ->
      let input =
            EventForm.FormInput
              eventTitle
              (renderDateForInput eventDate)
              eventLocation
              eventDescription
              eventFamilyAllowed
              [] -- TODO
       in return . LayoutStub "Veranstaltung Editieren" (Just Events) $
            div_ [class_ "container p-2"] $ do
              h1_ [class_ "h4 mb-3"] "Veranstaltung editieren"
              EventForm.render "Speichern" (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren") input EventForm.emptyState
  where
    renderDateForInput = Text.pack . Time.formatTime german "%d.%m.%Y %R"

handleUpdateEvent ::
  SQLite.Connection ->
  Wai.Request ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
handleUpdateEvent conn req eid@(EventId eventid) _ = do
  params <- parseParams req
  let input =
        EventForm.FormInput
          (Map.findWithDefault "" "eventTitleInput" params)
          (Map.findWithDefault "" "eventDateInput" params)
          (Map.findWithDefault "" "eventLocationInput" params)
          (Map.findWithDefault "" "eventDescriptionInput" params)
          (isJust $ Map.lookup "eventFamAllowedInput" params)
          [] -- TODO
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
    Right event@EventCreate {..} -> do
      updateEvent conn eid event
      return $
        LayoutStub "Veranstaltung Editieren" (Just Events) $
          success $ "Veranstaltung " <> eventCreateTitle <> " erfolgreich editiert"
