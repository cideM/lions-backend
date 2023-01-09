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
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as Aeson
import Data.ByteString (readFile)
import Data.Containers.ListUtils (nubOrd)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import qualified Events.Attachments.Saved as Saved
import qualified Events.Attachments.Temporary as Temporary
import Events.Event.Id (Id (..))
import Events.Reply.Reply (Reply)
import GHC.Generics
import qualified System.Directory
import System.FilePath ((</>))
import qualified UnliftIO
import Prelude hiding (id, readFile)

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
  (replies' :: [Reply]) <-
    (\e -> [i|error decoding replies JSON: #{e}|])
      `left` Aeson.eitherDecodeStrict (encodeUtf8 replies)

  (attachments' :: [Saved.FileName]) <-
    (\e -> [i|error decoding attachments JSON: #{e}|])
      `left` Aeson.eitherDecodeStrict (encodeUtf8 attachments)

  return (Id id, Event title date family desc loc (nubOrd replies') (nubOrd attachments'))

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
    App.HasEventStorage env,
    App.HasDb env
  ) =>
  Id ->
  m ()
delete (Id eventid) = do
  destinationDir <- asks App.getStorageDir
  conn <- asks App.getDb
  let eventidText = [i|#{eventid}|] :: Text
  let destDir = destinationDir </> (Text.unpack eventidText)
  exists <- liftIO $ System.Directory.doesDirectoryExist destDir
  when
    exists
    (liftIO $ System.Directory.removeDirectoryRecursive destDir)
  liftIO $ SQLite.execute conn "delete from event_replies where eventid = ?" [eventid]
  liftIO $ SQLite.execute conn "delete from event_attachments where eventid = ?" [eventid]
  liftIO $ SQLite.execute conn "delete from events where id = ?" [eventid]

save ::
  ( MonadIO m,
    MonadReader env m,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env
  ) =>
  Event Text ->
  [Temporary.Attachment] ->
  m Id
save Event {..} toUpload = do
  conn <- asks App.getDb

  id <- UnliftIO.withRunInIO $ \runInIO ->
    SQLite.withTransaction
      conn
      ( runInIO . liftIO $ do
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

          id <- SQLite.lastInsertRowId conn

          forM_ toUpload $ \Temporary.Attachment {..} -> do
            content <- readFile attachmentFilePath
            SQLite.execute
              conn
              [sql|insert into event_attachments (eventid, filename, content) values (?,?,?)|]
              (id, attachmentFileName, content)
            System.Directory.removeFile attachmentFilePath

          return id
      )

  return . Id $ fromIntegral id

update ::
  ( MonadIO m,
    MonadReader env m,
    App.HasEventStorage env,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env
  ) =>
  Id ->
  [Saved.FileName] ->
  [Temporary.Attachment] ->
  Event Text ->
  m ()
update (Id eventid) toDelete toUpload Event {..} = do
  conn <- asks App.getDb
  destinationDir <- asks App.getStorageDir

  UnliftIO.withRunInIO $ \runInIO ->
    SQLite.withTransaction
      conn
      ( runInIO $ do
          forM_ toDelete $ \savedAttachment -> do
            let filename = Saved.unFileName savedAttachment
            let eventidText = [i|#{eventid}|] :: Text

            -- try deleting old attachments stored on disk
            let destDir = destinationDir </> (Text.unpack eventidText)
            let dest = destDir </> (Text.unpack filename)
            exists <- liftIO $ System.Directory.doesFileExist dest
            when
              exists
              (liftIO $ System.Directory.removeFile dest)

            -- delete new attachments stored entirely in the DB, which also
            -- deletes the metadata for the old attachments
            liftIO $
              SQLite.execute
                conn
                [sql|delete from event_attachments where eventid = ? and filename = ? |]
                (eventid, Saved.unFileName savedAttachment)

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

          forM_ toUpload $ \Temporary.Attachment {..} -> do
            content <- liftIO $ readFile attachmentFilePath
            liftIO $
              SQLite.execute
                conn
                [sql|insert into event_attachments (eventid, filename, content) values (?,?,?)|]
                (eventid, attachmentFileName, content)
            liftIO $ System.Directory.removeFile attachmentFilePath
      )
