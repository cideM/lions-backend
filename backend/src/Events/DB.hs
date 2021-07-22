module Events.DB
  ( save,
    get,
    update,
    getAll,
    deleteReply,
    upsertReply,
    delete,
  )
where

import qualified App
import Control.Arrow (left)
import Control.Exception.Safe
import Events.Attachments.Saved as Saved
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Aeson as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import qualified Events.Reply as Events
import Database.SQLite.Simple.QQ (sql)
import qualified Events.Event as Events
import User.Types (UserId (..))
import Prelude hiding (id)

type EventRow = (Int, Text, Time.UTCTime, Bool, Text, Text, Text, Text)

save ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env
  ) =>
  Events.Create ->
  m Events.Id
save Events.Create {..} = do
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
      (createTitle, createDate, createFamilyAllowed, createDescription, createLocation)

  id <- liftIO $ SQLite.lastInsertRowId conn

  forM_ createFiles $ \filename ->
    liftIO $
      SQLite.execute
        conn
        [sql|insert into event_attachments (eventid, filename) values (?,?)|]
        (id, filename)

  return . Events.Id $ fromIntegral id

parseRow :: EventRow -> Either Text (Events.Id, Events.Event)
parseRow (id, title, date, family, desc, loc, replies, attachments) = do
  (attachments' :: [Saved.Attachment]) <- (\e -> [i|error decoding attachments JSON: #{e}|]) `left` Aeson.eitherDecodeStrict (encodeUtf8 attachments)
  (replies' :: [Events.Reply]) <- (\e -> [i|error decoding replies JSON: #{e}|]) `left` Aeson.eitherDecodeStrict (encodeUtf8 replies)
  return (Events.Id id, Events.Event title date family desc loc replies' attachments')

get ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Events.Id ->
  m (Maybe Events.Event)
get (Events.Id eventid) = do
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
                  else json_group_array(json_object('fileName',event_attachments.filename))
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
  m [(Events.Id, Events.Event)]
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
                  else json_group_array(json_object('fileName',event_attachments.filename))
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

deleteReply ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Events.Id ->
  UserId ->
  m ()
deleteReply (Events.Id eventid) (UserId userid) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "delete from event_replies where userid = ? and eventid = ?" [userid, eventid]

delete ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env
  ) =>
  Events.Id ->
  m ()
delete (Events.Id eventid) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "delete from event_replies where eventid = ?" [eventid]
  liftIO $ SQLite.execute conn "delete from event_attachments where eventid = ?" [eventid]
  liftIO $ SQLite.execute conn "delete from events where id = ?" [eventid]

update ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env
  ) =>
  Events.Id ->
  Events.Create ->
  m ()
update (Events.Id eventid) Events.Create {..} = do
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
      (createTitle, createDate, createFamilyAllowed, createDescription, createLocation, eventid)

  forM_ createFiles $ \filename ->
    liftIO $
      SQLite.execute
        conn
        [sql|insert into event_attachments (eventid, filename) values (?,?)|]
        (eventid, filename)

upsertReply ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env
  ) =>
  Events.Id ->
  Events.Reply ->
  m ()
upsertReply (Events.Id eventid) (Events.Reply coming _ (UserId userid) guests) = do
  conn <- asks App.getDb
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
      [userid, eventid, if coming then 1 else 0, guests, userid, eventid]
