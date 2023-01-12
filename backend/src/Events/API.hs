module Events.API
  ( getAll,
    get,
    getConfirmDelete,
    postDelete,
    getCreate,
    postCreate,
    postUpdate,
    EventID (..),
    getEdit,
    postUpdateEventReplies,
    attachmentsMiddleware,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.ByteString.Lazy as BL
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Events.DB
import Events.Event
import Events.HTML
import Events.Reply
import GHC.Exts (sortWith)
import qualified Katip as K
import Layout (LayoutStub (..), success)
import Locale (german)
import Lucid
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Status (status303)
import Network.Wai (pathInfo, responseLBS)
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Static as Static
import Network.Wai.Parse
  ( FileInfo (..),
    ParseRequestBodyOptions,
    defaultParseRequestBodyOptions,
    lbsBackEnd,
    parseRequestBodyEx,
    setMaxRequestFileSize,
    tempFileBackEnd,
  )
import Text.Read (readEither)
import qualified UnliftIO
import User.Session (Session (..))
import qualified User.Session
import qualified User.User as User
import qualified Wai
import Prelude hiding (id)

-- getAll is the handler for listing all events in a slightly abbreviated
-- overview.
getAll ::
  (MonadIO m, MonadReader env m, MonadThrow m, App.HasDb env) =>
  User.Session.Authenticated ->
  m LayoutStub
getAll auth = do
  conn <- asks App.getDb

  events <- dbFetchAllEvents conn

  now <- liftIO $ Time.getCurrentTime

  let (expired, upcoming) = partition ((>=) now . unEventDate . getEventDate . fst) events

  let upcomingSorted = sortWith (getEventDate . fst) upcoming

  -- dataForHtml is a view of the events that we got from the database that is
  -- tailored to what we display.
  let dataForHtml :: [(Event, Integer, Integer, Integer, Maybe Bool, Bool)]
      dataForHtml =
        let Session {sessionUserId} = User.Session.get' auth

            f (event, replies) =
              let rsvp = case [unReplyComing coming | (coming, _, userid) <- replies, userid == sessionUserId] of
                    [True] -> Just True -- Yes, I'm coming
                    [False] -> Just False -- No, I'm coming
                    _ -> Nothing -- I don't know

                  -- Calculate how many people accepted, declined and the total
                  -- number of people (coming + their guests)
                  countYes = fromIntegral $ length [() | (ReplyComing bool, _, _) <- replies, bool]
                  countNo = fromIntegral (length replies) - countYes
                  countTotal = sum [n | (ReplyComing bool, ReplyGuests n, _) <- replies, bool]

                  -- We mark events that lie in the past with a badge
                  isExpired = now >= (unEventDate $ getEventDate event)
               in (event, countYes, countNo, countTotal, rsvp, isExpired)
         in map f $ upcomingSorted ++ expired

  let currentUserIsAdmin = User.Session.isAdmin' auth

  return . LayoutStub "Veranstaltung" $ eventList dataForHtml currentUserIsAdmin

get ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Wai.Request ->
  EventID ->
  User.Session.Authenticated ->
  m (Maybe LayoutStub)
get req eventIdRouteParam auth = do
  conn <- asks App.getDb

  maybeEvent <- dbFetchEvent conn eventIdRouteParam
  case maybeEvent of
    Nothing -> return Nothing
    Just (event@(_, title, _, _, _, _), replies, attachments) -> do
      now <- liftIO $ Time.getCurrentTime

      -- On the view for a single event you can switch between looking at:
      -- - who's coming?
      -- - who's not coming?
      -- - are you coming?
      let queryString = Wai.queryString req
          whichReplyBoxToShow = case queryString of
            [("reply_box", Just "yes")] -> Yes
            [("reply_box", Just "no")] -> No
            _ -> Own

      let userIsAdmin = User.Session.isAdmin' auth

      let Session {sessionUserId} = User.Session.get' auth

      let isExpired = now >= (unEventDate $ getEventDate event)

      let ownReply = listToMaybe [reply | reply@Reply {..} <- replies, replyUserId == sessionUserId]

      return . Just $
        LayoutStub (unEventTitle title) $
          renderEvent event attachments replies ownReply isExpired userIsAdmin whichReplyBoxToShow

-- getConfirmDelete renders the confirmation dialogue when deleting an event.
getConfirmDelete ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m,
    MonadThrow m
  ) =>
  EventID ->
  User.Session.Admin ->
  m LayoutStub
getConfirmDelete eventIdRouteParam@(EventID eventid) _ = do
  conn <- asks App.getDb

  let htmlSuccess = \title -> do
        LayoutStub "Veranstaltung Löschen" $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-danger mb-4", role_ "alert"] $
                toHtml ("Veranstaltung " <> unEventTitle title <> " wirklich löschen?")
              form_
                [ action_ . Text.pack $ "/veranstaltungen/" <> show eventid <> "/loeschen",
                  method_ "post",
                  class_ "d-flex justify-content-center"
                ]
                $ button_ [class_ "btn btn-primary", type_ "submit"] "Ja, Veranstaltung löschen!"

  maybeTitle <- dbFetchEventTitle conn eventIdRouteParam

  case maybeTitle of
    Nothing -> throwString $ "delete event but no event for id: " <> show eventid
    Just title -> return (htmlSuccess title)

-- postDelete is the route handler for POST requests to delete an individual
-- event and all its replies and attachments.
postDelete ::
  ( MonadIO m,
    App.HasDb env,
    App.HasEventStorage env,
    MonadThrow m,
    MonadReader env m
  ) =>
  EventID ->
  User.Session.Admin ->
  m LayoutStub
postDelete eventIdRouteParam@(EventID eventid) _ = do
  conn <- asks App.getDb

  let htmlSuccess title = do
        LayoutStub "Veranstaltung" $
          success $
            "Veranstaltung " <> title <> " erfolgreich gelöscht"

  maybeTitle <- dbFetchEventTitle conn eventIdRouteParam

  case maybeTitle of
    Just title -> do
      dbDeleteEvent conn eventIdRouteParam
      return $ htmlSuccess $ unEventTitle title
    Nothing -> throwString $ "delete event but no event for id: " <> show eventid

-- getEdit is the route handler for a GET request to edit a single event,
-- including deleting attachments or uploading new ones.
getEdit ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m,
    MonadThrow m
  ) =>
  EventID ->
  User.Session.Admin ->
  m LayoutStub
getEdit eventIdRouteParam@(EventID eventid) _ = do
  conn <- asks App.getDb

  maybeEvent <- dbFetchEvent conn eventIdRouteParam

  case maybeEvent of
    Nothing -> throwString "Event not found"
    Just (event, _, filenames) -> do
      let (_, title, date, familyAllowed, location, description) = event

      let formattedDate = Text.pack $ Time.formatTime german "%d.%m.%Y %R" (unEventDate date)

      let userInput =
            FormInput
              (unEventTitle title)
              formattedDate
              (unEventLocation location)
              (unEventDescription description)
              familyAllowed

      return
        . LayoutStub "Veranstaltung Editieren"
        $ div_ [class_ "container p-2"]
        $ do
          h1_ [class_ "h4 mb-3"] "Veranstaltung editieren"
          eventForm
            "Speichern"
            (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren")
            [(name, True) | name <- filenames]
            userInput
            emptyState

-- getCreate is the handler for a GET request to the route that renders the
-- event form, which is also used for updating events.
getCreate :: (MonadIO m) => User.Session.Admin -> m LayoutStub
getCreate _ = do
  return . LayoutStub "Neue Veranstaltung" $
    div_ [class_ "container p-2"] $ do
      h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
      eventForm "Speichern" "/veranstaltungen/neu" [] emptyForm emptyState

fileUploadOpts :: ParseRequestBodyOptions
fileUploadOpts = setMaxRequestFileSize 100000000 defaultParseRequestBodyOptions

-- postCreate is the handler for a POST request to create a new event. You can
-- also upload attachment files when creating an event.
postCreate ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    K.KatipContext m,
    UnliftIO.MonadUnliftIO m,
    App.HasInternalState env,
    App.HasDb env
  ) =>
  Wai.Request ->
  User.Session.Admin ->
  m LayoutStub
postCreate request _ = do
  -- TODO: Improve
  -- This internalState should be short lived since we're creating it for each request, hopefully
  internalState <- asks App.getInternalState
  (params, files) <- liftIO $ parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) request

  K.katipAddNamespace "postCreate" $ do
    let paramsMap = Map.fromList [(decodeUtf8 k, decodeUtf8 v) | (k, v) <- params]
        fromParams key = Map.findWithDefault "" key paramsMap
        input =
          FormInput
            (fromParams "eventTitleInput")
            (fromParams "eventDateInput")
            (fromParams "eventLocationInput")
            (fromParams "eventDescriptionInput")
            (isJust $ Map.lookup "eventFamAllowedInput" paramsMap)

    case makeEvent input of
      Left state -> do
        return . LayoutStub "Neue Veranstaltung" $
          div_ [class_ "container p-2"] $ do
            h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
            eventForm "Speichern" "/veranstaltungen/neu" [] input state
      Right fields@(title, _, _, _, _) -> do
        conn <- asks App.getDb
        UnliftIO.withRunInIO $ \runInIO ->
          SQLite.withTransaction
            conn
            ( runInIO $ do
                eventid <- dbSaveEvent conn fields
                -- An empty string is the expected behavior when no file is selected, according to MDN
                -- > A file input's value attribute contains a string that represents the
                -- > path to the selected file(s). If no file is selected yet, the value is an
                -- > empty string ("")
                let newFiles = [file | file@(_, fileInfo) <- files, fileName fileInfo /= "\"\""]
                dbSaveAttachments conn eventid newFiles
            )

        return . LayoutStub "Neue Veranstaltung" $
          success $
            "Neue Veranstaltung " <> unEventTitle title <> " erfolgreich erstellt!"

-- postUpdate is the handler for a POST request to update a single event,
-- including deleting attachments or uploading new ones.
postUpdate ::
  ( MonadThrow m,
    MonadIO m,
    K.KatipContext m,
    App.HasInternalState env,
    UnliftIO.MonadUnliftIO m,
    MonadReader env m,
    App.HasDb env
  ) =>
  Wai.Request ->
  EventID ->
  User.Session.Admin ->
  m LayoutStub
postUpdate request eventIdRouteParam@(EventID eventid) _ = do
  conn <- asks App.getDb

  let htmlFail = \userInput formState -> do
        LayoutStub "Veranstaltung Editieren" $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            eventForm
              "Speichern"
              (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren")
              []
              userInput
              formState

  let htmlSuccess = \title ->
        LayoutStub "Veranstaltung Editieren" $
          success $
            "Veranstaltung " <> title <> " erfolgreich editiert"

  -- This internalState should be short lived since we're creating it for each request, hopefully
  internalState <- asks App.getInternalState

  (params, files) <- liftIO $ parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) request

  let paramsMap = Map.fromList [(decodeUtf8 k, decodeUtf8 v) | (k, v) <- params]

  let userInput =
        FormInput
          (Map.findWithDefault "" "eventTitleInput" paramsMap)
          (Map.findWithDefault "" "eventDateInput" paramsMap)
          (Map.findWithDefault "" "eventLocationInput" paramsMap)
          (Map.findWithDefault "" "eventDescriptionInput" paramsMap)
          (isJust $ Map.lookup "eventFamAllowedInput" paramsMap)

  case makeEvent userInput of
    Left state ->
      return $ htmlFail userInput state
    Right fields@(title, _, _, _, _) -> do
      let updateEventAndAttachments = do
            current <- dbFetchAttachments conn eventIdRouteParam

            let keep = [decodeUtf8 value | (key, value) <- params, key == "newFileCheckbox"]

            dbDeleteAttachments conn eventIdRouteParam [s | s <- current, not (elem s keep)]

            -- An empty string is the expected behavior when no file is selected, according to MDN
            -- > A file input's value attribute contains a string that represents the
            -- > path to the selected file(s). If no file is selected yet, the value is an
            -- > empty string ("")
            dbSaveAttachments conn eventIdRouteParam [file | file@(_, fileInfo) <- files, fileName fileInfo /= "\"\""]

            dbUpdateEvent conn eventIdRouteParam fields

      UnliftIO.withRunInIO $ \runInIO -> SQLite.withTransaction conn (runInIO $ updateEventAndAttachments)

      return $ htmlSuccess $ unEventTitle title

-- postUpdateEventReplies is the handler for POST requests that update the
-- event replies for the currently logged in user.
postUpdateEventReplies ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Wai.Request ->
  (Wai.Response -> m a) ->
  EventID ->
  User.Session.Authenticated ->
  m a
postUpdateEventReplies req send eventid auth = do
  conn <- asks App.getDb

  let parseComing :: Text -> Either Text (Maybe ReplyComing)
      parseComing "coming" = Right $ Just (ReplyComing True)
      parseComing "notcoming" = Right $ Just (ReplyComing False)
      parseComing "noreply" = Right Nothing
      parseComing s = Left [i|unknown coming value '#{s :: Text.Text}'|]

  let parseNumGuests :: Text -> Either Text ReplyGuests
      parseNumGuests "" = Right $ ReplyGuests 0
      parseNumGuests s =
        case readEither (Text.unpack s) of
          Left e -> Left [i|couldn't parse '#{s}' as number: #{e}|]
          Right n -> Right $ ReplyGuests n

  let User.Session.Session {..} = User.Session.get' auth

  (_, User.Profile {userEmail = userEmail}) <-
    User.get sessionUserId >>= \case
      Nothing -> throwString [i|"no user for userid from session #{sessionUserId}"|]
      Just v -> return v

  (params, _) <- liftIO $ parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd req

  let paramsMap = Map.fromList [(decodeUtf8 k, decodeUtf8 v) | (k, v) <- params]

  let tryParse = do
        coming <- parseComing $ Map.findWithDefault "" "reply" paramsMap
        guests <- parseNumGuests $ Map.findWithDefault "" "numberOfGuests" paramsMap
        return (coming, guests)

  case tryParse of
    Left err -> throwString $ Text.unpack err
    Right (coming, guests) -> do
      case coming of
        Nothing -> dbDeleteReply conn eventid sessionUserId
        Just r -> dbUpsertReply conn eventid (Reply r userEmail sessionUserId guests)

      send $
        Wai.responseLBS
          status303
          [("Location", encodeUtf8 [i|/veranstaltungen/#{unEventID eventid}|])]
          mempty

attachmentsMiddleware ::
  ( UnliftIO.MonadUnliftIO m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Wai.MiddlewareT m
attachmentsMiddleware next req send = do
  conn <- asks App.getDb
  case pathInfo req of
    ["events", eventid, filenameArg] -> do
      eventIdParsed <- case readEither $ Text.unpack eventid of
        Left e -> throwString [i|couldn't parse '#{eventid}' as number: #{e}|]
        Right n -> return n

      attachment <- dbFetchAttachmentsWithContent conn (EventID eventIdParsed) filenameArg

      case attachment of
        Nothing -> next req send
        Just (filename, content) -> do
          case content of
            Just actualContent -> do
              let mime = Static.mimeTypes Static.defaultOptions (Text.unpack filename)

              send $ responseLBS status200 [("Content-Type", mime)] (BL.fromChunks [actualContent])
            Nothing -> throwString $ "Found attachment but it's empty: " <> Text.unpack filenameArg
    _ -> do
      next req send
