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
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Resource (InternalState)
import qualified Data.ByteString.Char8 as C8
import Data.Foldable (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Events.Attachments as A
import qualified Events.DB
import qualified Events.Form as EventForm
import qualified Events.Preview
import qualified Events.Full
import qualified Events.Types as Events
import GHC.Exts (sortWith)
import Layout (ActiveNavLink (..), LayoutStub (..), success)
import Locale (german)
import Lucid
import Network.HTTP.Types (status303)
import qualified Network.Wai as Wai
import Network.Wai.Parse
  ( FileInfo (..),
    ParseRequestBodyOptions,
    defaultParseRequestBodyOptions,
    parseRequestBodyEx,
    setMaxRequestFileSize,
    tempFileBackEnd,
  )
import qualified Session.Auth as Session
import qualified Session.Types as Session
import qualified System.Directory
import Text.Read (readEither)
import User.Types (UserId (..), UserProfile (..))
import Wai (paramsToMap, parseParams)

getAll ::
  (MonadIO m, MonadReader env m, MonadThrow m, App.HasDb env) =>
  Session.Authenticated ->
  m LayoutStub
getAll auth = do
  events <- toEventList <$> Events.DB.getAll
  return . LayoutStub "Veranstaltungen" (Just Events) $ eventPreviewsHtml events
  where
    getOwnReplyFromEvent (eventid, event@Events.Event {..}) =
      let Session.UserSession {..} = Session.getSessionFromAuth auth
          reply = find ((==) userSessionUserId . Events.replyUserId) eventReplies
       in (eventid, event, reply)

    toEventList = map getOwnReplyFromEvent . sortWith (Down . Events.eventDate . snd)

    eventPreviewsHtml :: [(Events.Id, Events.Event, Maybe Events.Reply)] -> Html ()
    eventPreviewsHtml events =
      let userIsAdmin = Session.isUserAdmin auth
       in div_ [class_ "container"] $ do
            when userIsAdmin $
              a_ [class_ "mb-1 mb-2 btn btn-sm btn-primary", href_ "/veranstaltungen/neu", role_ "button"] "Neue Veranstaltung"
            h1_ [class_ "h3 mb-3"] "Veranstaltungen"
            div_ [class_ "row row-cols-1 row-cols-lg-2 gy-4 gx-lg-4"] $
              mapM_ (div_ [class_ "col"] . Events.Preview.render) events

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
  Session.Authenticated ->
  m a
postReply getUser req send eventid@(Events.Id eid) auth = do
  let Session.UserSession {..} = Session.getSessionFromAuth auth

  UserProfile {userEmail = userEmail} <-
    (liftIO $ getUser userSessionUserId) >>= \case
      Nothing -> throwString [i|"no user for userid from session #{userSessionUserId}"|]
      Just v -> return v

  (coming, numberOfGuests) <- liftIO $ parseParams'

  -- This is the core branching in this handler, everything else is just
  -- necessary edge cases and parsing inputs. The "Nothing" case is for when
  -- the user doesn't want commit to either coming or not coming, which I model
  -- as having no reply. The "Just" case is then either yes or no, which
  -- doesn't matter. What matters is that I now store that repl.
  case coming of
    Nothing -> Events.DB.deleteReply eventid userSessionUserId
    Just yesno ->
      Events.DB.upsertReply eventid $
        (Events.Reply yesno userEmail userSessionUserId numberOfGuests)

  send $ Wai.responseLBS status303 [("Location", encodeUtf8 [i|/veranstaltungen/#{eid}|])] mempty
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
  Session.Authenticated ->
  m (Maybe LayoutStub)
get eventid auth = do
  let userIsAdmin = Session.isUserAdmin auth
      Session.UserSession {..} = Session.getSessionFromAuth auth

  Events.DB.get eventid >>= \case
    Nothing -> return Nothing
    Just e@Events.Event {..} -> do
      let ownReply = find ((==) userSessionUserId . Events.replyUserId) eventReplies
      return . Just
        . LayoutStub
          eventTitle
          (Just Events)
        $ Events.Full.render (Events.Full.ShowAdminTools userIsAdmin) ownReply eventid e

getCreate :: (MonadIO m) => Session.AdminUser -> m LayoutStub
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
  Session.AdminUser ->
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
  Session.AdminUser ->
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
    App.HasEventStorage env,
    App.HasSessionEncryptionKey env,
    App.HasDb env
  ) =>
  InternalState ->
  Wai.Request ->
  Session.AdminUser ->
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
  body <- liftIO $ parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) req

  let params = paramsToMap $ fst body
      fromParams key = Map.findWithDefault "" key params

  (Events.FileActions {..}, encryptedFileInfos) <- A.parseRequest [] body

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
    Right newEvent@Events.Create {..} -> do
      eid <- Events.DB.save newEvent

      forM_ fileActionsUpload $ \FileInfo {..} -> do
        A.save eid fileContent (C8.unpack fileName)
        liftIO $ System.Directory.removeFile fileContent

      forM_ fileActionsDontUpload $ \FileInfo {..} -> do
        liftIO $ System.Directory.removeFile fileContent

      return . LayoutStub "Neue Veranstaltung" (Just Events) $
        success $ "Neue Veranstaltung " <> createTitle <> " erfolgreich erstellt!"

getEdit ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m,
    MonadThrow m
  ) =>
  Events.Id ->
  Session.AdminUser ->
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
    App.HasEventStorage env,
    App.HasSessionEncryptionKey env,
    MonadReader env m,
    App.HasDb env
  ) =>
  InternalState ->
  Wai.Request ->
  Events.Id ->
  Session.AdminUser ->
  m LayoutStub
postUpdate internalState req eid@(Events.Id eventid) _ = do
  body <- liftIO $ parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) req

  Events.DB.get eid >>= \case
    Nothing -> throwString $ "edit event but no event for id: " <> show eventid
    Just Events.Event {..} -> do
      (Events.FileActions {..}, encryptedFileInfos) <- A.parseRequest eventAttachments body

      let params = paramsToMap $ fst body
          fromParams key = Map.findWithDefault "" key params
          savedButDeleteCheckbox = zip (map Events.attachmentFileName fileActionsDelete) (repeat False)
          keepCheckbox = zip (map Events.attachmentFileName fileActionsKeep) (repeat True)
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
        Right event@Events.Create {..} -> do
          forM_ fileActionsUpload $ \FileInfo {..} -> do
            A.save eid fileContent (C8.unpack fileName)
            liftIO $ System.Directory.removeFile fileContent

          forM_ fileActionsDelete $ \Events.Attachment {..} -> do
            A.remove eid (Text.unpack attachmentFileName)

          forM_ fileActionsDontUpload $ \FileInfo {..} -> do
            liftIO $ System.Directory.removeFile fileContent

          Events.DB.update eid event
          return $
            LayoutStub "Veranstaltung Editieren" (Just Events) $
              success $ "Veranstaltung " <> createTitle <> " erfolgreich editiert"
