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
import qualified Data.String.Interpolate as Interpolate
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
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
  ( FileInfo (..),
    defaultParseRequestBodyOptions,
    parseRequestBodyEx,
    setMaxRequestFileSize,
    tempFileBackEnd,
  )
import qualified Session
import Text.Read (readEither)
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
  (UserId -> IO (Maybe UserProfile)) ->
  (EventId -> UserId -> IO ()) ->
  (EventId -> Reply -> IO ()) ->
  Wai.Request ->
  (Wai.Response -> IO a) ->
  EventId ->
  Session.Authenticated ->
  IO a
replyToEvent getUser deleteReply upsertReply req send eventid@(EventId i) auth = do
  let Session.UserSession {..} = Session.getSessionFromAuth auth

  UserProfile {userEmail = userEmail} <-
    getUser userSessionUserId >>= \case
      Nothing -> throwString [Interpolate.i|"no user for userid from session #{userSessionUserId}"|]
      Just v -> return v

  (coming, numberOfGuests) <- parseParams'

  -- This is the core branching in this handler, everything else is just
  -- necessary edge cases and parsing inputs. The "Nothing" case is for when
  -- the user doesn't want commit to either coming or not coming, which I model
  -- as having no reply. The "Just" case is then either yes or no, which
  -- doesn't matter. What matters is that I now store that repl.
  case coming of
    Nothing -> deleteReply eventid userSessionUserId
    Just yesno -> upsertReply eventid (Reply yesno userEmail userSessionUserId numberOfGuests)

  send $ Wai.responseLBS status303 [("Location", encodeUtf8 $ "/veranstaltungen/" <> Text.pack (show i))] mempty
  where
    parseParams' = do
      params <- parseParams req
      let replyParam = Map.lookup "reply" params
          numGuestsParam = Map.lookup "numberOfGuests" params
      coming <- maybe (throwString "no 'reply' param") parseComing replyParam
      numberOfGuests <- maybe (throwString "no 'numberOfGuests' param") parseNumGuests numGuestsParam
      return (coming, numberOfGuests)

    parseComing "coming" = return $ Just True
    parseComing "notcoming" = return $ Just False
    parseComing "noreply" = return Nothing
    parseComing s = throwString [Interpolate.i|unknown coming value '#{s :: Text.Text}'|]

    parseNumGuests "" = return 0
    parseNumGuests s =
      case readEither (Text.unpack s) of
        Left e -> throwString [Interpolate.i|couldn't parse '#{s}' as number: #{e}|]
        Right i' -> return i'

showEvent :: (EventId -> IO (Maybe Event)) -> EventId -> Session.Authenticated -> IO (Maybe LayoutStub)
showEvent getEvent eventid auth = do
  let (userIsAdmin, Session.UserSession {..}) = case auth of
        Session.IsAdmin (Session.AdminUser session) -> (True, session)
        Session.IsUser session -> (False, session)
  maybeevent <- getEvent eventid
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
  (EventId -> IO (Maybe Event)) ->
  (EventId -> IO ()) ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
handleDeleteEvent getEvent deleteEvent eventid _ = do
  maybeEvent <- getEvent eventid
  case maybeEvent of
    Nothing -> return . LayoutStub "Fehler" (Just Events) $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just event -> do
      deleteEvent eventid
      return $
        LayoutStub "Veranstaltung" (Just Events) $
          success $ "Veranstaltung " <> eventTitle event <> " erfolgreich gelöscht"

showDeleteEventConfirmation :: (EventId -> IO (Maybe Event)) -> EventId -> Session.AdminUser -> IO LayoutStub
showDeleteEventConfirmation getEvent eid@(EventId eventid) _ = do
  -- TODO: Duplicated
  getEvent eid >>= \case
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

  -- Here's what the body will look like
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
      checkboxes = parseFileUploadBody body
      fromParams key = Map.findWithDefault "" key params
      input =
        EventForm.FormInput
          (fromParams "eventTitleInput")
          (fromParams "eventDateInput")
          (fromParams "eventLocationInput")
          (fromParams "eventDescriptionInput")
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
  where
    -- File size 100MB
    fileUploadOpts = setMaxRequestFileSize 100000000 defaultParseRequestBodyOptions

    parseFileUploadBody body =
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

showEditEventForm ::
  (EventId -> IO (Maybe Event)) ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
showEditEventForm getEvent eid@(EventId eventid) _ = do
  getEvent eid >>= \case
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
  (EventId -> EventCreate -> IO ()) ->
  Wai.Request ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
handleUpdateEvent updateEvent req eid@(EventId eventid) _ = do
  params <- parseParams req
  let fromParams key = Map.findWithDefault "" key params
      input =
        EventForm.FormInput
          (fromParams "eventTitleInput")
          (fromParams "eventDateInput")
          (fromParams "eventLocationInput")
          (fromParams "eventDescriptionInput")
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
      updateEvent eid event
      return $
        LayoutStub "Veranstaltung Editieren" (Just Events) $
          success $ "Veranstaltung " <> eventCreateTitle <> " erfolgreich editiert"
