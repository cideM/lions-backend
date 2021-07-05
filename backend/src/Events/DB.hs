module Events.DB (EventDb (..), newEventDb)
where

import Control.Arrow (left)
import Control.Exception.Safe
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import Events.Domain (Event (..), EventAttachment (..), EventCreate (..), EventId (..), Reply (..))
import User.Types (UserId (..))
import Prelude hiding (id)

type EventRow = (Int, Text, Time.UTCTime, Bool, Text, Text, Text, Text)

data EventDb = EventDb
  { eventDbCreate :: EventCreate -> IO EventId,
    eventDbGet :: EventId -> IO (Maybe Event),
    eventDbAll :: IO [(EventId, Event)],
    eventDbDeleteReply :: EventId -> UserId -> IO (),
    eventDbDelete :: EventId -> IO (),
    eventDbUpdate :: EventId -> EventCreate -> IO (),
    eventDbUpsertReply :: EventId -> Reply -> IO ()
  }

newEventDb :: SQLite.Connection -> EventDb
newEventDb conn =
  EventDb
    { eventDbCreate = createEvent conn,
      eventDbGet = getEvent conn,
      eventDbAll = getAll conn,
      eventDbDeleteReply = deleteReply conn,
      eventDbDelete = deleteEvent conn,
      eventDbUpdate = updateEvent conn,
      eventDbUpsertReply = upsertReply conn
    }

createEvent :: SQLite.Connection -> EventCreate -> IO EventId
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
  id <- SQLite.lastInsertRowId conn
  forM_ eventCreateFiles $ \filename ->
    SQLite.execute conn [sql|insert into event_attachments (eventid, filename) values (?,?)|] (id, filename)
  return . EventId $ fromIntegral id

createEventFromDb :: EventRow -> Either Text (EventId, Event)
createEventFromDb (id, title, date, family, desc, loc, replies, attachments) = do
  (attachments' :: [EventAttachment]) <- (\e -> [i|error decoding attachments JSON: #{e}|]) `left` Aeson.eitherDecodeStrict (encodeUtf8 attachments)
  (replies' :: [Reply]) <- (\e -> [i|error decoding replies JSON: #{e}|]) `left` Aeson.eitherDecodeStrict (encodeUtf8 replies)
  return (EventId id, Event title date family desc loc replies' attachments')

getEvent :: SQLite.Connection -> EventId -> IO (Maybe Event)
getEvent conn (EventId eventid) = do
  ([row] :: [EventRow]) <-
    SQLite.query
      conn
      [sql|
        select events.id as eventid,
               title,
               date,
               family_allowed,
               description,
               location,
               (case (count(rep.coming))
                 when 0 then "[]"
                 else json_group_array(
                        json_object('userId',rep.userid,
                                    'coming',json(case rep.coming when 1 then "true" else "false" end),
                                    'guests',rep.guests,
                                    'userEmail', users.email))
               end) as replies,
               (case (count(event_attachments.filename))
                  when 0 then "[]"
                  else json_group_array(json_object('fileName',event_attachments.filename))
               end) as attachments
        from events
        left join event_attachments on events.id = event_attachments.eventid
        left join event_replies rep on events.id = rep.eventid
        left join users on userid = users.id
        where events.id = ?
      |]
      [eventid]
  case createEventFromDb row of
    Left e -> throwString $ [i|couldn't parse event with id #{eventid} from DB: #{e}|]
    Right ok -> return $ Just $ snd ok

getAll :: SQLite.Connection -> IO [(EventId, Event)]
getAll conn = do
  (rows :: [EventRow]) <-
    SQLite.query_
      conn
      [sql|
        select events.id as eventid,
               title,
               date,
               family_allowed,
               description,
               location,
               (case (count(rep.coming))
                 when 0 then "[]"
                 else json_group_array(
                        json_object('userId',rep.userid,
                                    'coming',json(case rep.coming when 1 then "true" else "false" end),
                                    'guests',rep.guests,
                                    'userEmail', users.email))
               end) as replies,
               '[]' as attachments
        from events
        left join event_replies rep on events.id = rep.eventid
        left join users on userid = users.id
        group by events.id
      |]
  case traverse createEventFromDb rows of
    Left e -> throwString $ "couldn't parse events from DB: " <> show e
    Right parsed -> return parsed

deleteReply :: SQLite.Connection -> EventId -> UserId -> IO ()
deleteReply conn (EventId eventid) (UserId userid) =
  SQLite.execute conn "delete from event_replies where userid = ? and eventid = ?" [userid, eventid]

deleteEvent :: SQLite.Connection -> EventId -> IO ()
deleteEvent conn (EventId eventid) = do
  SQLite.execute conn "delete from event_replies where eventid = ?" [eventid]
  SQLite.execute conn "delete from event_attachments where eventid = ?" [eventid]
  SQLite.execute conn "delete from events where id = ?" [eventid]

updateEvent :: SQLite.Connection -> EventId -> EventCreate -> IO ()
updateEvent conn (EventId eventid) EventCreate {..} = do
  SQLite.execute conn [sql|delete from event_attachments where eventid = ?|] [eventid]
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
  forM_ eventCreateFiles $ \filename ->
    SQLite.execute conn [sql|insert into event_attachments (eventid, filename) values (?,?)|] (eventid, filename)

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
