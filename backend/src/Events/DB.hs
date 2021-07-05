module Events.DB (EventDb (..), newEventDb, createEvent)
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
import qualified Events.Types as Events
import User.Types (UserId (..))
import Prelude hiding (id)

type EventRow = (Int, Text, Time.UTCTime, Bool, Text, Text, Text, Text)

data EventDb = EventDb
  { eventDbCreate :: Events.Create -> IO Events.Id,
    eventDbGet :: Events.Id -> IO (Maybe Events.Event),
    eventDbAll :: IO [(Events.Id, Events.Event)],
    eventDbDeleteReply :: Events.Id -> UserId -> IO (),
    eventDbDelete :: Events.Id -> IO (),
    eventDbUpdate :: Events.Id -> Events.Create -> IO (),
    eventDbUpsertReply :: Events.Id -> Events.Reply -> IO ()
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

createEvent :: SQLite.Connection -> Events.Create -> IO Events.Id
createEvent conn Events.Create {..} = do
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
    (createTitle, createDate, createFamilyAllowed, createDescription, createLocation)
  id <- SQLite.lastInsertRowId conn
  forM_ createFiles $ \filename ->
    SQLite.execute conn [sql|insert into event_attachments (eventid, filename) values (?,?)|] (id, filename)
  return . Events.Id $ fromIntegral id

createEventFromDb :: EventRow -> Either Text (Events.Id, Events.Event)
createEventFromDb (id, title, date, family, desc, loc, replies, attachments) = do
  (attachments' :: [Events.Attachment]) <- (\e -> [i|error decoding attachments JSON: #{e}|]) `left` Aeson.eitherDecodeStrict (encodeUtf8 attachments)
  (replies' :: [Events.Reply]) <- (\e -> [i|error decoding replies JSON: #{e}|]) `left` Aeson.eitherDecodeStrict (encodeUtf8 replies)
  return (Events.Id id, Events.Event title date family desc loc replies' attachments')

getEvent :: SQLite.Connection -> Events.Id -> IO (Maybe Events.Event)
getEvent conn (Events.Id eventid) = do
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

getAll :: SQLite.Connection -> IO [(Events.Id, Events.Event)]
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
               (case (count(event_attachments.filename))
                  when 0 then "[]"
                  else json_group_array(json_object('fileName',event_attachments.filename))
               end) as attachments
        from events
        left join event_replies rep on events.id = rep.eventid
        left join users on userid = users.id
        left join event_attachments on events.id = event_attachments.eventid
        group by events.id
      |]
  case traverse createEventFromDb rows of
    Left e -> throwString $ "couldn't parse events from DB: " <> show e
    Right parsed -> return parsed

deleteReply :: SQLite.Connection -> Events.Id -> UserId -> IO ()
deleteReply conn (Events.Id eventid) (UserId userid) =
  SQLite.execute conn "delete from event_replies where userid = ? and eventid = ?" [userid, eventid]

deleteEvent :: SQLite.Connection -> Events.Id -> IO ()
deleteEvent conn (Events.Id eventid) = do
  SQLite.execute conn "delete from event_replies where eventid = ?" [eventid]
  SQLite.execute conn "delete from event_attachments where eventid = ?" [eventid]
  SQLite.execute conn "delete from events where id = ?" [eventid]

updateEvent :: SQLite.Connection -> Events.Id -> Events.Create -> IO ()
updateEvent conn (Events.Id eventid) Events.Create {..} = do
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
    (createTitle, createDate, createFamilyAllowed, createDescription, createLocation, eventid)
  forM_ createFiles $ \filename ->
    SQLite.execute conn [sql|insert into event_attachments (eventid, filename) values (?,?)|] (eventid, filename)

upsertReply :: SQLite.Connection -> Events.Id -> Events.Reply -> IO ()
upsertReply conn (Events.Id eventid) (Events.Reply coming _ (UserId userid) guests) =
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
