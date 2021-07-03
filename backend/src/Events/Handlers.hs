module Events.Handlers
  ( showAllEvents,
    showCreateEvent,
    showEvent,
    showDeleteEventConfirmation,
    handleDeleteEvent,
    getNewCheckboxes,
    handleCreateEvent,
    uploadAttachmentToS3,
    handleUpdateEvent,
    showEditEventForm,
    replyToEvent,
  )
where

import Control.Exception.Safe
import Control.Monad (when, forM_)
import Control.Monad.Trans.Resource (InternalState)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (find)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import qualified Data.String.Interpolate as Interpolate
import Data.Text (Text)
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
    Param,
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

uploadAttachmentToS3 :: Text -> FilePath -> IO S3.PutObject
uploadAttachmentToS3 name filePath = do
  contents <- BS.readFile filePath
  let key = S3.ObjectKey name
  return $ S3.putObject "lions-achern-event-attachments" key $ Hashed $ toHashed contents

getNewCheckboxes ::
  (ByteString -> IO ByteString) ->
  (ByteString -> Maybe ByteString) ->
  ([Param], [(ByteString, FileInfo FilePath)]) ->
  IO (Either Text.Text [(Text.Text, Text, ByteString, Bool)])
getNewCheckboxes encrypt decrypt body = do
  -- All files newly added with this particular request. There appears to
  -- be some bug, not sure if it's the browser or the server, where I get a
  -- file that has an empty string as a name. Hence the weird filter call.
  let newFilesFiltered = filter ((/=) "\"\"" . fileName) . map snd $ snd body

  -- Keep track of the human readable file name as a checkbox label later.
  (newFilesProcessed :: [(Text, Text, ByteString)]) <-
    -- Each file info is turned into a string separated by colons and
    -- then encrypted so it can be safely sent to the client later.  But
    -- we return a tuple where the first element is the label mentioned
    -- above.
    traverse
      ( \FileInfo {..} -> do
          encrypted <- encrypt [Interpolate.i|#{fileName}:#{fileContentType}:#{fileContent}|]
          return (decodeUtf8 fileName, Text.pack fileContent, encrypted)
      )
      newFilesFiltered

  -- Unchecked checkboxes are not sent with the form, so we keep track
  -- of them through hidden input fields, which all have the name
  -- "allFiles". The value of these input fields is a string consisting
  -- of three parts: file name, file type and file location on the
  -- server. All are concatenated with colons **and encrypted** so we
  -- don't expose internals to the client. We do this because we only
  -- know about the storage location of any upload when it's uploaded
  -- initially. But the file paths have nothing to do with the file
  -- names, so we can't retroactively identify temp files belonging to
  -- any form.
  let encryptedHiddenInputs =
        map snd
          . filter ((==) "allFiles" . fst)
          $ fst body

  -- We need to turn the encrypted strings from the previous request into
  -- checkboxes with human readable labels. That's why we need to decrypt them
  -- so we can at least get the first value in the colon delimited list.
  case traverse processEncrypted encryptedHiddenInputs of
    Nothing -> return $ Left "couldn't decrypt hidden inputs"
    Just decrypted -> do
      -- Extract the name from each encrypted string and create a tuple of
      -- (label, encrypted), just like we did with the new files
      let -- Join old files and new files, always in the form of tuples
          allFileNames = newFilesProcessed ++ decrypted

          -- checked checkboxes from current request
          checked = map (decodeUtf8 . snd) . filter ((==) "newFileCheckbox" . fst) $ fst body

          -- every new file starts out as checked
          newFileNames = map (\(name, _, _) -> name) newFilesProcessed

          allChecked = checked ++ newFileNames

      -- For each file generate a checkbox and optionally mark it as checked.
      return
        . Right
        $ map (\(name, filepath, encrypted) -> (name, filepath, encrypted, name `elem` allChecked)) allFileNames
  where
    parseDecryptedString :: ByteString -> (Text, Text, Text)
    parseDecryptedString s =
      let [filename, filetype, filepath] = Text.split (':' ==) $ decodeUtf8 s
       in (filename, filetype, filepath)

    processEncrypted :: ByteString -> Maybe (Text, Text, ByteString)
    processEncrypted s = do
      (name, _, filepath) <- parseDecryptedString <$> decrypt s
      return (name, filepath, s)

handleCreateEvent ::
  (ByteString -> IO ByteString) ->
  (ByteString -> Maybe ByteString) ->
  (EventCreate -> IO ()) ->
  InternalState ->
  (Text -> FilePath -> IO S3.PutObjectResponse) ->
  Wai.Request ->
  Session.AdminUser ->
  IO LayoutStub
handleCreateEvent encrypt decrypt createEvent internalState uploadFile req _ = do
  -- There's no point in trying to catch an exception arising from a payload
  -- that's too large. The connection will be closed, so no more bytes are read
  -- by the server, and the browser will likely just show a connection reset
  -- browser error. Meaning the user will have no idea what's going on.
  -- Therefore the limit is set really high. If I want to enforce a lower limit
  -- but also show nice errors that means always reading the entire request
  -- payload and then doing a check for its size. For now I just won't enforce
  -- any file limit other than the hard 100MB.
  body <- parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) req

  let params = paramsToMap $ fst body
      fromParams key = Map.findWithDefault "" key params

  checkboxes <-
    getNewCheckboxes encrypt decrypt body >>= \case
      Left e -> throwString $ Text.unpack e
      Right v -> return v

  let input =
        EventForm.FormInput
          (fromParams "eventTitleInput")
          (fromParams "eventDateInput")
          (fromParams "eventLocationInput")
          (fromParams "eventDescriptionInput")
          (isJust $ Map.lookup "eventFamAllowedInput" params)
          checkboxes

  -- We get the params and turn that into FormInput. This MUST INCLUDE the encrypted paths
  -- If it's the first submission, the file paths must be generated from the File part of body
  -- If it's not the first submission those paths will come from the hidden input fields
  -- Do fileName:filePath:fileType so I only deal with a single
  -- I already do this for checkboxes so all I got to do is amend checkboxes
  -- Then I can iterate over the decrypted strings in "Right" and upload
  case EventForm.makeEvent input of
    Left state -> do
      return . LayoutStub "Neue Veranstaltung" (Just Events) $
        div_ [class_ "container p-2"] $ do
          h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
          EventForm.render "Speichern" "/veranstaltungen/neu" input state
    Right newEvent@EventCreate {..} -> do
      createEvent newEvent
      forM_ checkboxes (\(name, filepath, _, _) -> uploadFile name $ Text.unpack filepath)
      return . LayoutStub "Neue Veranstaltung" (Just Events) $
        success $ "Neue Veranstaltung " <> eventCreateTitle <> " erfolgreich erstellt!"
  where
    -- File size 100MB
    fileUploadOpts = setMaxRequestFileSize 100000000 defaultParseRequestBodyOptions

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
