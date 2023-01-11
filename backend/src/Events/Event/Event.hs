module Events.Event.Event
  ( Event (..),
    get,
    getAll,
    save,
    delete,
  )
where

import Control.Arrow (left)
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import Data.Containers.ListUtils (nubOrd)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import Events.Event.Id (Id (..))
import Events.Reply.Reply (Reply)
import GHC.Generics
import Prelude hiding (id)

data Event = Event
  { eventTitle :: Text,
    eventDate :: Time.UTCTime,
    eventFamilyAllowed :: Bool,
    eventDescription :: Text,
    eventLocation :: Text,
    eventReplies :: [Reply],
    eventAttachments :: [Text]
  }
  deriving (Show, Eq, Generic)

type EventRow = (Int, Text, Time.UTCTime, Bool, Text, Text, Text, Text)

parseRow :: EventRow -> Either Text (Id, Event)
parseRow (id, title, date, family, desc, loc, replies, attachments) = do
  (replies' :: [Reply]) <-
    (\e -> [i|error decoding replies JSON: #{e}|])
      `left` Aeson.eitherDecodeStrict (encodeUtf8 replies)

  (attachments' :: [Text]) <-
    (\e -> [i|error decoding attachments JSON: #{e}|])
      `left` Aeson.eitherDecodeStrict (encodeUtf8 attachments)

  return (Id id, Event title date family desc loc (nubOrd replies') (nubOrd attachments'))

get :: (MonadIO m, MonadThrow m) => SQLite.Connection -> Id -> m (Maybe Event)
get conn (Id eventid) = do
  rows <-
    liftIO $
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
                  else json_group_array(event_attachments.filename)
               end) as attachments
        from events
        left join event_attachments on events.id = event_attachments.eventid
        left join event_replies rep on events.id = rep.eventid
        left join users on userid = users.id
        where events.id = ?
      |]
        [eventid]

  case rows of
    [row :: EventRow] ->
      case parseRow row of
        Left e -> throwString $ [i|couldn't parse event with id #{eventid} from DB: #{e}|]
        Right ok -> return $ Just $ snd ok
    r -> throwString [i|Unexpected rows for getting event: #{r}|]

getAll :: (MonadIO m, Monad m, MonadThrow m) => SQLite.Connection -> m [(Id, Event)]
getAll conn = do
  rows <-
    liftIO $
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
                  else json_group_array(event_attachments.filename)
               end) as attachments
        from events
        left join event_replies rep on events.id = rep.eventid
        left join users on userid = users.id
        left join event_attachments on events.id = event_attachments.eventid
        group by events.id
      |]
  case traverse parseRow rows of
    Left e -> throwString $ "couldn't parse events from DB: " <> show e
    Right parsed -> return parsed

delete :: (MonadIO m) => SQLite.Connection -> Id -> m ()
delete conn (Id eventid) = do
  liftIO $ SQLite.execute conn "delete from event_replies where eventid = ?" [eventid]
  liftIO $ SQLite.execute conn "delete from event_attachments where eventid = ?" [eventid]
  liftIO $ SQLite.execute conn "delete from events where id = ?" [eventid]

save :: (MonadIO m) => SQLite.Connection -> Event -> m Id
save conn Event {..} = do
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
      (eventTitle, eventDate, eventFamilyAllowed, eventDescription, eventLocation)

  id <- liftIO $ SQLite.lastInsertRowId conn

  return . Id $ fromIntegral id
