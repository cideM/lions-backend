{-# LANGUAGE DeriveAnyClass #-}

module Events.DB where

import Control.Exception.Safe
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (zipWith4)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple (NamedParam (..))
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.QQ (sql)
import Events.Event
import Events.Reply
import GHC.Generics
import Network.Wai.Parse (File, FileInfo (..))
import qualified User.Email
import qualified User.Id
import Prelude hiding (id)

data DbFetchAllEventsRow = DbFetchAllEventsRow
  { id :: EventID,
    title :: EventTitle,
    date :: EventDate,
    familyAllowed :: Integer,
    location :: EventLocation,
    description :: EventDescription,
    comingStr :: Maybe Text,
    guestsStr :: Maybe Text,
    userIdsStr :: Maybe Text,
    emailsStr :: Maybe Text
  }
  deriving (FromRow, Generic)

dbFetchAllEvents ::
  ( MonadThrow m,
    MonadCatch m,
    MonadIO m
  ) =>
  SQLite.Connection ->
  m [(Event, [Reply])]
dbFetchAllEvents conn = do
  handleAny (\e -> throwString $ "error fetching all events: " <> show e) $ do
    rows <-
      liftIO $
        SQLite.query_
          conn
          [sql|
                select events.id, title, date, family_allowed, location, description,
                       group_concat(event_replies.coming) as coming,
                       group_concat(event_replies.guests) as guests,
                       group_concat(event_replies.userid) as userids,
                       group_concat(users.email) as useremails
                from events
                left join event_replies on events.id = eventid
                left join users on users.id = event_replies.userid
                group by events.id
              |]

    let parseCommaInts :: Text -> [Integer]
        parseCommaInts "" = []
        parseCommaInts s = map (read . Text.unpack) $ Text.splitOn "," s

    let parseRow :: DbFetchAllEventsRow -> Either Text (Event, [Reply])
        parseRow DbFetchAllEventsRow {..} = do
          -- Each event reply or RSVP consists of 3 values:
          -- - whether you're coming or not, which is 1 or 0 in the DB, but a bool in Haskell
          -- - the number of guests you're bringing
          -- - the user ID of the current reply/RSVP
          --
          -- We're constructing them by joining the event_replies table
          -- to event, and then we aggregate the value of **each column**
          -- by concatenating them with a comma. It gives us a
          -- column-centric view. On the Haskell side, we parse each list
          -- of comma-separated integers and then we equip them with
          -- their respective newtype wrappers.
          coming <- traverse parseReplyComingFromNumber . parseCommaInts $ fromMaybe "" comingStr

          emails <- case emailsStr of
            Nothing -> return []
            Just "" -> return []
            Just s -> traverse User.Email.parseFromText $ Text.splitOn "," s

          let userIds = map (User.Id.Id . fromIntegral) . parseCommaInts $ fromMaybe "" userIdsStr
              guests = map ReplyGuests . parseCommaInts $ fromMaybe "" guestsStr

          let event = (id, title, date, familyAllowed == 1, location, description)
              replies = zipWith4 Reply coming emails userIds guests

          return (event, replies)

    case traverse parseRow rows of
      Left err -> throwString $ "error parsing rows: " <> Text.unpack err
      Right parsedRows -> return parsedRows

dbFetchEvent ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  EventID ->
  m
    ( Maybe
        ( Event,
          [Reply],
          [Text]
        )
    )
dbFetchEvent conn eventid = do
  (eventRows :: [(EventID, EventTitle, EventDate, Integer, EventLocation, EventDescription)]) <-
    liftIO $
      SQLite.query
        conn
        [sql|select id, title, date, family_allowed, location, description
             from events where id = ?|]
        [eventid]

  case eventRows of
    [(id, title, date, familyAllowed, location, description)] -> do
      replies <-
        liftIO $
          SQLite.query
            conn
            [sql|select coming, email, userid, guests
                 from event_replies
                 left join users on users.id = userid
                 where eventid = ?|]
            [id]

      (attachments :: [[Text]]) <-
        liftIO $
          SQLite.query
            conn
            [sql|select filename from event_attachments where eventid = ?|]
            [id]

      return $ Just ((id, title, date, familyAllowed == 1, location, description), replies, concat attachments)
    [] -> return Nothing
    r -> throwString [i|Unexpected rows for getting event: #{r}|]

dbFetchEventTitle :: (MonadThrow m, MonadIO m) => SQLite.Connection -> EventID -> m (Maybe EventTitle)
dbFetchEventTitle conn eventid = do
  (rows :: [[EventTitle]]) <- liftIO $ SQLite.query conn "select title from events where id = ?" [eventid]

  return $ case rows of
    [[title]] -> Just title
    _ -> Nothing

dbDeleteEvent :: (MonadThrow m, MonadIO m) => SQLite.Connection -> EventID -> m ()
dbDeleteEvent conn eventid = liftIO $ do
  SQLite.execute conn "delete from event_replies where eventid = ?" [eventid]
  SQLite.execute conn "delete from event_attachments where eventid = ?" [eventid]
  SQLite.execute conn "delete from events where id = ?" [eventid]

dbUpdateEvent ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  EventID ->
  (EventTitle, EventDate, Bool, EventLocation, EventDescription) ->
  m ()
dbUpdateEvent conn eventid (title, date, familyAllowed, location, description) = do
  liftIO $
    SQLite.executeNamed
      conn
      [sql| update events set title = :title, date = :date, family_allowed = :family_allowed,
                              description = :description, location = :location 
            where id = :id|]
      [ ":title" := title,
        ":date" := date,
        ":family_allowed" := familyAllowed,
        ":description" := description,
        ":location" := location,
        ":id" := eventid
      ]

dbSaveAttachments ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  EventID ->
  [File FilePath] ->
  m ()
dbSaveAttachments conn eventid files = do
  forM_ files $ \(_, FileInfo {fileName, fileContent}) -> do
    content <- liftIO $ BS.readFile fileContent
    liftIO $
      SQLite.execute
        conn
        [sql|insert into event_attachments (eventid, filename, content) values (?,?,?)|]
        (eventid, decodeUtf8 $ fileName, content)

dbDeleteAttachments ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  EventID ->
  [Text] ->
  m ()
dbDeleteAttachments conn eventid filenames = do
  forM_ filenames $ \fileName -> do
    liftIO $
      SQLite.execute
        conn
        [sql|delete from event_attachments where eventid = ? and filename = ? |]
        (eventid, fileName)

dbFetchAttachmentsWithContent ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  EventID ->
  Text ->
  m (Maybe (Text, Maybe ByteString))
dbFetchAttachmentsWithContent conn eventid filename = do
  rows <-
    liftIO $
      SQLite.query
        conn
        [sql|select filename, content from event_attachments where eventid = ? and filename = ?|]
        (eventid, filename)

  case rows of
    [] -> return Nothing
    [v] -> return $ Just v
    r -> throwString [i|Unexpected rows for getting event attachment: #{r}|]

dbFetchAttachments ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  EventID ->
  m [Text]
dbFetchAttachments conn eventid = do
  (rows :: [[Text]]) <-
    liftIO $
      SQLite.query
        conn
        [sql|select filename from event_attachments where eventid = ?|]
        [eventid]

  return $ concat rows

dbSaveEvent ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  (EventTitle, EventDate, Bool, EventLocation, EventDescription) ->
  m EventID
dbSaveEvent conn (title, date, familyAllowed, location, description) = do
  liftIO $
    SQLite.executeNamed
      conn
      [sql|insert into events ( title, date, family_allowed, description, location)
           values (:title, :date, :familyAllowed, :description, :location)|]
      [ ":title" := title,
        ":date" := date,
        ":family_allowed" := familyAllowed,
        ":description" := description,
        ":location" := location
      ]

  id <- liftIO $ SQLite.lastInsertRowId conn

  return . EventID $ fromIntegral id

dbDeleteReply ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  EventID ->
  User.Id.Id ->
  m ()
dbDeleteReply conn eid userid = do
  liftIO $
    SQLite.execute
      conn
      "delete from event_replies where userid = ? and eventid = ?"
      (userid, eid)

dbUpsertReply ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  EventID ->
  Reply ->
  m ()
dbUpsertReply conn eid Reply {..} = do
  liftIO $
    SQLite.execute
      conn
      [sql|
          insert into event_replies (userid, eventid, coming, guests)
          values (?,?,?,?)
          on conflict (userid,eventid) do update set
            coming=excluded.coming,
            guests=excluded.guests
          where userid = ? and eventid = ?
        |]
      (replyUserId, eid, replyComing, replyGuests, replyUserId, eid)
