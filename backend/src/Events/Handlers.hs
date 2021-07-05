module Events.Handlers
  ( showAllEvents,
    showCreateEvent,
    showEvent,
    showDeleteEventConfirmation,
    removeAllAttachments,
    handleDeleteEvent,
    FileActions (..),
    getFileActions,
    handleCreateEvent,
    saveAttachment,
    removeAttachment,
    handleUpdateEvent,
    showEditEventForm,
    replyToEvent,
  )
where

import Control.Exception.Safe
import Control.Monad (forM_, when)
import Control.Monad.Trans.Resource (InternalState)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Foldable (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import qualified Data.String.Interpolate as Interpolate
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import Events.Domain (Event (..), EventAttachment (..), EventCreate (..), EventId (..), Reply (..))
import qualified Events.EventForm as EventForm
import qualified Events.Preview
import qualified Events.SingleEvent
import GHC.Exts (sortWith)
import Layout (ActiveNavLink (..), LayoutStub (..), success)
import Locale (german)
import Lucid
import Network.HTTP.Types (status303)
import qualified Network.Wai as Wai
import Network.Wai.Parse
  ( FileInfo (..),
    Param,
    ParseRequestBodyOptions,
    defaultParseRequestBodyOptions,
    parseRequestBodyEx,
    setMaxRequestFileSize,
    tempFileBackEnd,
  )
import qualified Session
import qualified System.Directory
import System.FilePath ((</>))
import Text.Read (readEither)
import User.Types (UserId (..), UserProfile (..))
import Wai (paramsToMap, parseParams)

toEventList :: UserId -> [(EventId, Event)] -> [(EventId, Event, Maybe Reply)]
toEventList userid = map getOwnReplyFromEvent . sortEvents
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

showAllEvents :: IO [(EventId, Event)] -> Session.Authenticated -> IO LayoutStub
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
    Just yesno -> upsertReply eventid $ (Reply yesno userEmail userSessionUserId numberOfGuests)

  send $ Wai.responseLBS status303 [("Location", encodeUtf8 $ "/veranstaltungen/" <> Text.pack (show i))] mempty
  where
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
    parseComing s = Left [Interpolate.i|unknown coming value '#{s :: Text.Text}'|]

    parseNumGuests :: Text -> Either Text Int
    parseNumGuests "" = Right 0
    parseNumGuests s =
      case readEither (Text.unpack s) of
        Left e -> Left [Interpolate.i|couldn't parse '#{s}' as number: #{e}|]
        Right i' -> Right i'

showEvent :: (EventId -> IO (Maybe Event)) -> EventId -> Session.Authenticated -> IO (Maybe LayoutStub)
showEvent getEvent eventid auth = do
  let userIsAdmin = Session.isUserAdmin auth
      Session.UserSession {..} = Session.getSessionFromAuth auth
  getEvent eventid >>= \case
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
  (EventId -> IO ()) ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
handleDeleteEvent getEvent removeAll deleteEvent eventid _ = do
  maybeEvent <- getEvent eventid
  case maybeEvent of
    Nothing -> return . LayoutStub "Fehler" (Just Events) $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just event -> do
      removeAll eventid
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

saveAttachment :: FilePath -> EventId -> FilePath -> FilePath -> IO ()
saveAttachment destinationDir (EventId eid) source destinationFileName = do
  let destDir = destinationDir </> [Interpolate.i|#{eid}|]
  System.Directory.createDirectoryIfMissing True destDir

  let dest = destDir </> destinationFileName

  System.Directory.copyFile source dest

removeAllAttachments :: FilePath -> EventId -> IO ()
removeAllAttachments destinationDir (EventId eid) =
  System.Directory.removeDirectoryRecursive $ destinationDir </> show eid

removeAttachment :: FilePath -> EventId -> FilePath -> IO ()
removeAttachment destinationDir (EventId eid) filename =
  System.Directory.removeFile $ destinationDir </> show eid </> filename

parseDecryptedString :: ByteString -> (Text, Text, Text)
parseDecryptedString s =
  let [filename, filetype, filepath] = Text.split (':' ==) $ decodeUtf8 s
   in (filename, filetype, filepath)

getEncryptedFileInfo ::
  (ByteString -> Maybe ByteString) ->
  ([Param], [(ByteString, FileInfo FilePath)]) ->
  Either Text.Text [(FileInfo FilePath)]
getEncryptedFileInfo decrypt body =
  let encryptedHiddenInputs = map snd . filter ((==) "allFiles" . fst) $ fst body
   in case traverse decrypt encryptedHiddenInputs of
        Nothing -> Left "couldn't decrypt hidden inputs"
        Just decrypted ->
          let parsed = map parseDecryptedString decrypted
              fileinfos = map (\(name, filetype, path) -> FileInfo (encodeUtf8 name) (encodeUtf8 filetype) (Text.unpack path)) parsed
           in Right fileinfos

getNewFileInfo :: ([Param], [(ByteString, FileInfo FilePath)]) -> [(FileInfo FilePath)]
getNewFileInfo = filter ((/=) "\"\"" . fileName) . map snd . snd

encryptFileInfo ::
  (ByteString -> IO ByteString) ->
  [(FileInfo FilePath)] ->
  IO [ByteString]
encryptFileInfo encrypt =
  traverse (\FileInfo {..} -> encrypt [Interpolate.i|#{fileName}:#{fileContentType}:#{fileContent}|])

getCheckedFromBody :: ([Param], [(ByteString, FileInfo FilePath)]) -> [Text]
getCheckedFromBody = map (decodeUtf8 . snd) . filter ((==) "newFileCheckbox" . fst) . fst

data FileActions = FileActions
  { fileActionsKeep :: [EventAttachment],
    fileActionsDelete :: [EventAttachment],
    fileActionsDontUpload :: [FileInfo FilePath],
    fileActionsUpload :: [FileInfo FilePath]
  }
  deriving (Show, Eq)

getFileActions ::
  (ByteString -> IO ByteString) ->
  (ByteString -> Maybe ByteString) ->
  [EventAttachment] ->
  ([Param], [(ByteString, FileInfo FilePath)]) ->
  IO (FileActions, [ByteString])
getFileActions encrypt decrypt alreadySaved body = do
  pastFileInfos <- case getEncryptedFileInfo decrypt body of
    Left e -> throwString $ Text.unpack e
    Right v -> return v

  let newFileInfos = getNewFileInfo body
      checked = getCheckedFromBody body
      keep = filter (flip elem checked . eventAttachmentFileName) alreadySaved
      savedButDelete = filter (flip notElem checked . eventAttachmentFileName) alreadySaved
      notSavedAndDelete = filter (flip notElem checked . decodeUtf8 . fileName) pastFileInfos
      upload = newFileInfos ++ (filter (flip elem checked . decodeUtf8 . fileName) pastFileInfos)

  encryptedFileInfos <- encryptFileInfo encrypt $ pastFileInfos ++ newFileInfos

  let actions = FileActions keep savedButDelete notSavedAndDelete upload

  return (actions, encryptedFileInfos)

fileUploadOpts :: ParseRequestBodyOptions
fileUploadOpts = setMaxRequestFileSize 100000000 defaultParseRequestBodyOptions

handleCreateEvent ::
  (ByteString -> IO ByteString) ->
  (ByteString -> Maybe ByteString) ->
  (EventCreate -> IO EventId) ->
  InternalState ->
  (EventId -> FilePath -> FilePath -> IO ()) ->
  Wai.Request ->
  Session.AdminUser ->
  IO LayoutStub
handleCreateEvent encrypt decrypt createEvent internalState saveFile req _ = do
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

  (FileActions {..}, encryptedFileInfos) <- getFileActions encrypt decrypt [] body

  let notSavedAndDeleteCheckbox = map (\FileInfo {..} -> (decodeUtf8 fileName, False)) fileActionsDontUpload
      uploadCheckbox = zip (map (decodeUtf8 . fileName) fileActionsUpload) (repeat True)
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
    Right newEvent@EventCreate {..} -> do
      eid <- createEvent newEvent

      forM_ fileActionsUpload $ \FileInfo {..} -> do
        saveFile eid fileContent (C8.unpack fileName)
        System.Directory.removeFile fileContent

      forM_ fileActionsDontUpload $ \FileInfo {..} -> do
        System.Directory.removeFile fileContent

      return . LayoutStub "Neue Veranstaltung" (Just Events) $
        success $ "Neue Veranstaltung " <> eventCreateTitle <> " erfolgreich erstellt!"

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
              (zip (map eventAttachmentFileName eventAttachments) (repeat True))
              []
       in return . LayoutStub "Veranstaltung Editieren" (Just Events) $
            div_ [class_ "container p-2"] $ do
              h1_ [class_ "h4 mb-3"] "Veranstaltung editieren"
              EventForm.render "Speichern" (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren") input EventForm.emptyState
  where
    renderDateForInput = Text.pack . Time.formatTime german "%d.%m.%Y %R"

handleUpdateEvent ::
  (EventId -> EventCreate -> IO ()) ->
  (EventId -> IO (Maybe Event)) ->
  (ByteString -> IO ByteString) ->
  (ByteString -> Maybe ByteString) ->
  (EventId -> FilePath -> IO ()) ->
  (EventId -> FilePath -> FilePath -> IO ()) ->
  InternalState ->
  Wai.Request ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
handleUpdateEvent
  updateEvent
  getEvent
  encrypt
  decrypt
  removeFile
  saveFile
  internalState
  req
  eid@(EventId eventid)
  _ = do
    body <- parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) req

    getEvent eid >>= \case
      Nothing -> throwString $ "edit event but no event for id: " <> show eventid
      Just Event {..} -> do
        (FileActions {..}, encryptedFileInfos) <- getFileActions encrypt decrypt eventAttachments body

        let params = paramsToMap $ fst body
            fromParams key = Map.findWithDefault "" key params
            savedButDeleteCheckbox = zip (map eventAttachmentFileName fileActionsDelete) (repeat False)
            keepCheckbox = zip (map eventAttachmentFileName fileActionsKeep) (repeat True)
            notSavedAndDeleteCheckbox = map (\FileInfo {..} -> (decodeUtf8 fileName, False)) fileActionsDontUpload
            uploadCheckbox = zip (map (decodeUtf8 . fileName) fileActionsUpload) (repeat True)
            checkboxes = keepCheckbox ++ savedButDeleteCheckbox ++ notSavedAndDeleteCheckbox ++ uploadCheckbox
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
          Right event@EventCreate {..} -> do
            forM_ fileActionsUpload $ \FileInfo {..} -> do
              saveFile eid fileContent (C8.unpack fileName)
              System.Directory.removeFile fileContent

            forM_ fileActionsDelete $ \EventAttachment {..} -> do
              removeFile eid (Text.unpack eventAttachmentFileName)

            forM_ fileActionsDontUpload $ \FileInfo {..} -> do
              System.Directory.removeFile fileContent

            updateEvent eid event
            return $
              LayoutStub "Veranstaltung Editieren" (Just Events) $
                success $ "Veranstaltung " <> eventCreateTitle <> " erfolgreich editiert"
