module Events.DB
  ( getEvent,
    getAll,
    deleteReply,
    createEvent,
    GetEventRow (..),
    createAndAggregateEventsFromDb,
    updateEvent,
    upsertReply,
    deleteEvent,
  )
where

import Control.Exception.Safe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.QQ (sql)
import Events.Domain (Event (..), EventCreate (..), EventId (..), Reply (..))
import Text.Email.Validate (emailAddress)
import User.Types (UserEmail (..), UserId (..))
import Prelude hiding (id)

data GetEventRow = GetEventRow
  { _eventId :: Int,
    _eventTitle :: Text,
    _eventDate :: Time.UTCTime,
    _eventFamilyAllowed :: Bool,
    _eventDescription :: Text,
    _eventLocation :: Text,
    _eventReplyUserId :: Maybe Int,
    _eventReplyComing :: Maybe Bool,
    _eventReplyNumGuests :: Maybe Int,
    _eventReplyEmail :: Maybe Text
  }
  deriving (Show)

instance FromRow GetEventRow where
  fromRow =
    GetEventRow
      <$> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field

createEvent :: SQLite.Connection -> EventCreate -> IO ()
createEvent conn EventCreate {..} = do
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
    (eventCreateTitle, eventCreateDate, eventCreateFamilyAllowed, eventCreateDescription, eventCreateLocation)

-- | Converts a single row to an event. The idea is that you can just map over
-- rows and then merge them all into one event
createEventFromDb :: GetEventRow -> Either Text (EventId, Event)
createEventFromDb GetEventRow {..} =
  -- These properties we just pass through, only thing that needs special
  -- handling is the reply
  let event = Event _eventTitle _eventDate _eventFamilyAllowed _eventDescription _eventLocation
   in case getReplyFromRow of
        Nothing -> Right $ (EventId _eventId, event [])
        Just (uid, coming, email, numguests) ->
          case emailAddress (encodeUtf8 email) of
            Nothing -> Left $ "couldn't parse email " <> email
            Just parsedEmail ->
              Right $ (EventId _eventId, event [Reply coming (UserEmail parsedEmail) (UserId uid) numguests])
  where
    -- Unfortunately I currently get four separate maybe fields from the SQL
    -- query when in fact they're all related. So it's all or nothing, they're
    -- all Just or all Nothing. Not sure if it's worth refactoring.
    getReplyFromRow = do
      (,,,)
        <$> _eventReplyUserId
        <*> _eventReplyComing
        <*> _eventReplyEmail
        <*> _eventReplyNumGuests

-- Just a little wrapper around the above createEventFromDb function so I can
-- unit test this because it does have quite a lot of logic now.
createAndAggregateEventsFromDb :: [GetEventRow] -> Either Text (Map EventId Event)
createAndAggregateEventsFromDb rows = M.fromListWith (<>) <$> traverse createEventFromDb rows

getEvent :: SQLite.Connection -> EventId -> IO (Maybe Event)
getEvent conn eid@(EventId eventid) = do
  (rows :: [GetEventRow]) <-
    SQLite.query
      conn
      [sql|
        select events.id as eventid,
               title,
               date,
               family_allowed,
               description,
               location,
               userid,
               coming,
               guests,
               users.email as email
        from events
        left join event_replies on events.id = eventid
        left join users on userid = users.id
        where events.id = ?
      |]
      [eventid]
  case createAndAggregateEventsFromDb rows of
    Left e -> throwString $ [i|couldn't parse event with id #{eventid} from DB: #{e}|]
    Right ok ->
      case M.lookup eid ok of
        Nothing -> throwString "parsed GetEventRow but no map entry"
        Just v -> return $ Just v

-- Gets all events from the DB and aggregates them by event ID by merging all
-- user replies for a given event and making them unique on the odd chance that
-- some SQL query error results in multiple rows with the same result. I don't
-- have the patience to test that case right now through property testing.
getAll :: SQLite.Connection -> IO (Map EventId Event)
getAll conn = do
  (rows :: [GetEventRow]) <-
    SQLite.query_
      conn
      [sql|
        select events.id as eventid,
               title,
               date,
               family_allowed,
               description,
               location,
               userid,
               coming,
               guests,
               users.email as email
        from events
        left join event_replies on events.id = eventid
        left join users on userid = users.id
      |]
  case createAndAggregateEventsFromDb rows of
    Left e -> throwString $ "couldn't parse events from DB: " <> show e
    Right parsed -> return parsed

deleteReply :: SQLite.Connection -> EventId -> UserId -> IO ()
deleteReply conn (EventId eventid) (UserId userid) =
  SQLite.execute conn "delete from event_replies where userid = ? and eventid = ?" [userid, eventid]

deleteEvent :: SQLite.Connection -> EventId -> IO ()
deleteEvent conn (EventId eventid) = do
  SQLite.execute conn "delete from event_replies where eventid = ?" [eventid]
  SQLite.execute conn "delete from events where id = ?" [eventid]

updateEvent :: SQLite.Connection -> EventId -> EventCreate -> IO ()
updateEvent conn (EventId eventid) EventCreate {..} =
  SQLite.execute
    conn
    [sql|
        update events
        set
          title = ?,
          date = ?,
          family_allowed = ?,
          description = ?,
          location = ?
        where id = ?
      |]
    (eventCreateTitle, eventCreateDate, eventCreateFamilyAllowed, eventCreateDescription, eventCreateLocation, eventid)

-- Inserts a new reply or updates the existing one.
upsertReply :: SQLite.Connection -> EventId -> Reply -> IO ()
upsertReply conn (EventId eventid) (Reply coming _ (UserId userid) guests) =
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
    [userid, eventid, if coming then 1 else 0, guests, userid, eventid]
