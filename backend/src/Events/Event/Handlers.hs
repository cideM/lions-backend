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
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Foldable (find)
import Data.List (partition, (\\))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import qualified Events.Event.Event as Event
import qualified Events.Event.Form as EventForm
import qualified Events.Event.Full as Event.Full
import qualified Events.Event.Id as Event
import qualified Events.Event.Preview as Event.Preview
import Events.Reply.Reply (Reply (..))
import qualified Events.Reply.Reply as Reply
import GHC.Exts (sortWith)
import qualified Katip as K
import Layout (LayoutStub (..), infoBox, success)
import Locale (german)
import Lucid
import qualified Network.Wai as Wai
import Network.Wai.Parse
  ( FileInfo (..),
    ParseRequestBodyOptions,
    defaultParseRequestBodyOptions,
    parseRequestBodyEx,
    setMaxRequestFileSize,
    tempFileBackEnd,
  )
import qualified UnliftIO
import qualified User.Session
import Prelude hiding (id)

getAll ::
  (MonadIO m, MonadReader env m, MonadThrow m, App.HasDb env) =>
  User.Session.Authenticated ->
  m LayoutStub
getAll auth = do
  conn <- asks App.getDb
  events <- Event.getAll conn

  now <- liftIO $ Time.getCurrentTime

  let (past, upcoming) = partition ((>=) now . Event.eventDate . snd) events
      upcomingSorted = sortWith (Event.eventDate . snd) upcoming
      formatted = map (formatEventData now) (upcomingSorted ++ past)

  return . LayoutStub "Veranstaltungen" $ eventPreviewsHtml formatted
  where
    formatEventData now (eventid, event@Event.Event {..}) =
      let User.Session.Session {..} = User.Session.get' auth
          userReply = find ((==) sessionUserId . Reply.replyUserId) eventReplies
          isExpired = Event.Preview.IsExpired (now >= eventDate)
       in (eventid, event, userReply, isExpired)

    -- Ideally there's zero markup in the handler files.
    eventPreviewsHtml ::
      [ ( Event.Id,
          Event.Event,
          Maybe Reply,
          Event.Preview.IsExpired
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
                (mapM_ (div_ [class_ "col"] . Event.Preview.render) events)

get ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Wai.Request ->
  Event.Id ->
  User.Session.Authenticated ->
  m (Maybe LayoutStub)
get req eventid auth = do
  conn <- asks App.getDb
  now <- liftIO $ Time.getCurrentTime
  let queryString = Wai.queryString req
      whichReplyBoxToShow = case queryString of
        [("reply_box", Just "yes")] -> Event.Full.Yes
        [("reply_box", Just "no")] -> Event.Full.No
        _ -> Event.Full.Own

  let userIsAdmin = User.Session.isAdmin' auth
      User.Session.Session {..} = User.Session.get' auth

  Event.get conn eventid >>= \case
    Nothing -> return Nothing
    Just e@Event.Event {..} -> do
      let isExpired = Event.Full.IsExpired (now >= eventDate)
          ownReply = find ((==) sessionUserId . Reply.replyUserId) eventReplies
          isAdmin = Event.Full.ShowAdminTools userIsAdmin
          html = Event.Full.render whichReplyBoxToShow isExpired isAdmin ownReply eventid e

      return . Just $ LayoutStub eventTitle html

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
  conn <- asks App.getDb
  (Event.get conn eid) >>= \case
    Nothing -> throwString $ "delete event but no event for id: " <> show eventid
    Just event -> do
      return . LayoutStub "Veranstaltung Löschen" $
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
  conn <- asks App.getDb
  maybeEvent <- Event.get conn eventid
  case maybeEvent of
    Nothing -> throwString $ "delete event but no event for id: " <> show eventid
    Just event -> do
      Event.delete conn eventid
      return $
        LayoutStub "Veranstaltung" $
          success $
            "Veranstaltung " <> Event.eventTitle event <> " erfolgreich gelöscht"

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
  conn <- asks App.getDb
  (Event.get conn eid) >>= \case
    Nothing -> throwString $ "edit event but no event for id: " <> show eventid
    Just Event.Event {..} ->
      let input =
            -- (zip (map Saved.unFileName eventAttachments) (repeat True))
            -- TODO: checkboxes?
            EventForm.FormInput
              eventTitle
              (renderDateForInput eventDate)
              eventLocation
              eventDescription
              eventFamilyAllowed
       in return . LayoutStub "Veranstaltung Editieren" $
            div_ [class_ "container p-2"] $ do
              h1_ [class_ "h4 mb-3"] "Veranstaltung editieren"
              EventForm.render
                "Speichern"
                (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren")
                [] -- TODO?
                input
                EventForm.emptyState
  where
    renderDateForInput = Text.pack . Time.formatTime german "%d.%m.%Y %R"

getCreate :: (MonadIO m) => User.Session.Admin -> m LayoutStub
getCreate _ = do
  return . LayoutStub "Neue Veranstaltung" $
    div_ [class_ "container p-2"] $ do
      h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
      EventForm.render "Speichern" "/veranstaltungen/neu" [] EventForm.emptyForm EventForm.emptyState

fileUploadOpts :: ParseRequestBodyOptions
fileUploadOpts = setMaxRequestFileSize 100000000 defaultParseRequestBodyOptions

postCreate ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    K.KatipContext m,
    UnliftIO.MonadUnliftIO m,
    App.HasEventStorage env,
    App.HasSessionEncryptionKey env,
    App.HasInternalState env,
    App.HasDb env
  ) =>
  Wai.Request ->
  User.Session.Admin ->
  m LayoutStub
postCreate request _ = do
  -- This internalState should be short lived since we're creating it for each request, hopefully
  internalState <- asks App.getInternalState
  (params, files) <- liftIO $ parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) request

  K.katipAddNamespace "postCreate" $ do
    let paramsMap = Map.fromList [(decodeUtf8 k, decodeUtf8 v) | (k, v) <- params]
        fromParams key = Map.findWithDefault "" key paramsMap
        input =
          EventForm.FormInput
            (fromParams "eventTitleInput")
            (fromParams "eventDateInput")
            (fromParams "eventLocationInput")
            (fromParams "eventDescriptionInput")
            (isJust $ Map.lookup "eventFamAllowedInput" paramsMap)

    case EventForm.makeEvent input of
      Left state -> do
        return . LayoutStub "Neue Veranstaltung" $
          div_ [class_ "container p-2"] $ do
            h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
            EventForm.render "Speichern" "/veranstaltungen/neu" [] input state
      Right fields@(title, _, _, _, _) -> do
        conn <- asks App.getDb
        UnliftIO.withRunInIO $ \runInIO ->
          SQLite.withTransaction
            conn
            ( runInIO $ do
                eventid <- save fields
                -- An empty string is the expected behavior when no file is selected, according to MDN
                -- > A file input's value attribute contains a string that represents the
                -- > path to the selected file(s). If no file is selected yet, the value is an
                -- > empty string ("")
                let newFiles = [file | file@(_, fileInfo) <- files, fileName fileInfo /= "\"\""]
                storeAttachments eventid newFiles
            )

        return . LayoutStub "Neue Veranstaltung" $
          success $
            "Neue Veranstaltung " <> title <> " erfolgreich erstellt!"
  where
    storeAttachments eventid files = do
      conn <- asks App.getDb
      forM_ files $ \(_, fileInfo) -> do
        liftIO $
          SQLite.execute
            conn
            [sql|insert into event_attachments (eventid, filename, content) values (?,?,?)|]
            (eventid, decodeUtf8 $ fileName fileInfo, fileContent fileInfo)

    save (title, date, location, description, familyAllowed) = do
      conn <- asks App.getDb
      liftIO $
        SQLite.execute
          conn
          [sql|
              insert into events (
                title,
                date,
                family_allowed,
                description,
                location
              ) values (?,?,?,?,?)
            |]
          (title, date, familyAllowed, description, location)

      id <- liftIO $ SQLite.lastInsertRowId conn
      return id

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
  Event.Id ->
  User.Session.Admin ->
  m LayoutStub
postUpdate request (Event.Id eventid) _ = do
  -- This internalState should be short lived since we're creating it for each request, hopefully
  internalState <- asks App.getInternalState
  (params, files) <- liftIO $ parseRequestBodyEx fileUploadOpts (tempFileBackEnd internalState) request

  let paramsMap = Map.fromList [(decodeUtf8 k, decodeUtf8 v) | (k, v) <- params]
      fromParams key = Map.findWithDefault "" key paramsMap
      input =
        EventForm.FormInput
          (fromParams "eventTitleInput")
          (fromParams "eventDateInput")
          (fromParams "eventLocationInput")
          (fromParams "eventDescriptionInput")
          (isJust $ Map.lookup "eventFamAllowedInput" paramsMap)

  case EventForm.makeEvent input of
    Left state ->
      return $
        LayoutStub "Veranstaltung Editieren" $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            EventForm.render
              "Speichern"
              (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren")
              []
              input
              state
    Right fields@(title, _, _, _, _) -> do
      conn <- asks App.getDb
      UnliftIO.withRunInIO $ \runInIO ->
        SQLite.withTransaction
          conn
          ( runInIO $ do
              -- An empty string is the expected behavior when no file is selected, according to MDN
              -- > A file input's value attribute contains a string that represents the
              -- > path to the selected file(s). If no file is selected yet, the value is an
              -- > empty string ("")
              let newFiles = [file | file@(_, fileInfo) <- files, fileName fileInfo /= "\"\""]
              storeAttachments newFiles

              -- TODO: Change name of newFileCheckbox
              -- need to flatten [[filename]]
              current <- liftIO $ concat <$> SQLite.query conn [sql|select filename from event_attachments where eventid = ?|] [eventid]
              let keep = [decodeUtf8 value | (key, value) <- params, key == "newFileCheckbox"]
              deleteAttachments (current \\ keep)

              updateEvent fields
          )
      return $
        LayoutStub "Veranstaltung Editieren" $
          success $
            "Veranstaltung " <> title <> " erfolgreich editiert"
  where
    updateEvent (title, date, location, description, familyAllowed) = do
      conn <- asks App.getDb
      liftIO $
        SQLite.execute
          conn
          [sql| update events set
                title = ?,
                date = ?,
                family_allowed = ?,
                description = ?,
                location = ?
                where id = ?
            |]
          (title, date, familyAllowed, description, location, eventid)

    storeAttachments files = do
      conn <- asks App.getDb
      forM_ files $ \(_, fileInfo) -> do
        liftIO $
          SQLite.execute
            conn
            [sql|insert into event_attachments (eventid, filename, content) values (?,?,?)|]
            (eventid, decodeUtf8 $ fileName fileInfo, fileContent fileInfo)

    deleteAttachments filenames = do
      conn <- asks App.getDb
      forM_ filenames $ \fileName -> do
        liftIO $
          SQLite.execute
            conn
            [sql|delete from event_attachments where eventid = ? and filename = ? |]
            (eventid, fileName)
