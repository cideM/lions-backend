module Events.Event.Event
  ( Event (..),
    get,
    getAll,
    save,
    update,
    delete,
  )
where

import qualified App
import Control.Arrow (left)
import Control.Exception.Safe
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import qualified Events.Attachments.Saved as Saved
import Events.Event.Id (Id (..))
import Events.Reply.Reply (Reply)
import GHC.Generics
import Prelude hiding (id)

data Event attachment = Event
  { eventTitle :: Text,
    eventDate :: Time.UTCTime,
    eventFamilyAllowed :: Bool,
    eventDescription :: Text,
    eventLocation :: Text,
    eventReplies :: [Reply],
    eventAttachments :: [attachment]
  }
  deriving (Show, Eq, Generic)

instance (ToJSON attachment) => ToJSON (Event attachment) where
  toEncoding = genericToEncoding defaultOptions

type EventRow = (Int, Text, Time.UTCTime, Bool, Text, Text, Text, Text)

parseRow :: EventRow -> Either Text (Id, Event Saved.FileName)
parseRow (id, title, date, family, desc, loc, replies, attachments) = do
  (replies' :: [Reply]) <- (\e -> [i|error decoding replies JSON: #{e}|]) `left` Aeson.eitherDecodeStrict (encodeUtf8 replies)
  (attachments' :: [Saved.FileName]) <- (\e -> [i|error decoding attachments JSON: #{e}|]) `left` Aeson.eitherDecodeStrict (encodeUtf8 attachments)
  return (Id id, Event title date family desc loc replies' attachments')

get ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Id ->
  m (Maybe (Event Saved.FileName))
get (Id eventid) = do
  conn <- asks App.getDb
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

getAll ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  m [(Id, Event Saved.FileName)]
getAll = do
  conn <- asks App.getDb
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

delete ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env
  ) =>
  Id ->
  m ()
delete (Id eventid) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "delete from event_replies where eventid = ?" [eventid]
  liftIO $ SQLite.execute conn "delete from event_attachments where eventid = ?" [eventid]
  liftIO $ SQLite.execute conn "delete from events where id = ?" [eventid]

save ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env
  ) =>
  Event Text ->
  m Id
save Event {..} = do
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
      (eventTitle, eventDate, eventFamilyAllowed, eventDescription, eventLocation)

  id <- liftIO $ SQLite.lastInsertRowId conn

  forM_ eventAttachments $ \filename ->
    liftIO $
      SQLite.execute
        conn
        [sql|insert into event_attachments (eventid, filename) values (?,?)|]
        (id, filename)

  return . Id $ fromIntegral id

update ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env
  ) =>
  Id ->
  Event Text ->
  m ()
update (Id eventid) Event {..} = do
  conn <- asks App.getDb

  liftIO $ SQLite.execute conn [sql|delete from event_attachments where eventid = ?|] [eventid]

  liftIO $
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
      (eventTitle, eventDate, eventFamilyAllowed, eventDescription, eventLocation, eventid)

  forM_ eventAttachments $ \filename ->
    liftIO $
      SQLite.execute
        conn
        [sql|insert into event_attachments (eventid, filename) values (?,?)|]
        (eventid, filename)
