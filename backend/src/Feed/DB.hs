module Feed.DB where

import qualified App
import Control.Exception.Safe
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import Feed.Message
import Network.Wai.Parse (File, FileInfo (..))
import Prelude hiding (id)

type GetRow = (Int, Text, Time.UTCTime, Text)

-- | Returns the MOST RECENT welcome message, if there is one
get ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  m (Maybe (Message Text, [Text]))
get (Id id) = do
  conn <- asks App.getDb

  (rows :: [GetRow]) <-
    liftIO $
      SQLite.query
        conn
        [sql|SELECT welcome_text.id
                  , welcome_text.content
                  , date
                  , json_group_array(filename)
                      filter (where filename is not null) as filenames
             FROM welcome_text
             LEFT JOIN feed_attachments on postid = welcome_text.id
             WHERE welcome_text.id = ?|]
        [id]

  case rows of
    [(mid, msg, createdAt, filenamesJSON)] -> do
      let tryFilenames = Aeson.eitherDecodeStrict (encodeUtf8 filenamesJSON)

      case tryFilenames of
        Left err -> throwString err
        Right filenames -> do
          let message = Message (Id mid) msg createdAt
          return $ Just (message, filenames)
    [] -> return Nothing
    other -> do
      let err = "unexpected result from DB for welcome message" <> show other
      return $ throwString err

type GetAllRow = (Int, Text, Time.UTCTime, Text)

-- | Returns all feed posts in chronological order
getAll ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  m [(Message Text, [Text])]
getAll = do
  conn <- asks App.getDb

  (rows :: [GetAllRow]) <-
    liftIO $
      SQLite.query_
        conn
        [sql|SELECT welcome_text.id
                  , welcome_text.content
                  , date
                  , json_group_array(filename)
                      filter (where filename is not null) as filenames
             FROM welcome_text
             LEFT JOIN feed_attachments on postid = welcome_text.id
             GROUP BY welcome_text.id
             ORDER BY date DESC
        |]

  case rows of
    posts -> do
      parsed <-
        traverse
          ( \(id, content, createdAt, filenamesJSON) -> do
              let tryFilenames = Aeson.eitherDecodeStrict (encodeUtf8 filenamesJSON)

              case tryFilenames of
                Left err -> throwString err
                Right filenames -> do
                  let message = Message (Id id) content createdAt

                  pure (message, filenames)
          )
          posts

      pure parsed

delete ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  m ()
delete (Id id) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "DELETE FROM welcome_text WHERE id = ?" [id]

save ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Text ->
  Time.UTCTime ->
  m Id
save msg date = do
  conn <- asks App.getDb

  id <- liftIO $ do
    SQLite.execute
      conn
      "INSERT INTO welcome_text (content, date) VALUES (?, ?)"
      (msg, date)

    SQLite.lastInsertRowId conn

  pure . Id $ fromIntegral id

update ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  Text ->
  Time.UTCTime ->
  m ()
update (Id id) newMsg newDate = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "UPDATE welcome_text SET content = ?, date = ? WHERE id = ?" (newMsg, newDate, id)

saveAttachments ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  Id ->
  [File FilePath] ->
  m ()
saveAttachments conn postId files = do
  forM_ files $ \(_, FileInfo {fileName, fileContent}) -> do
    content <- liftIO $ BS.readFile fileContent
    liftIO $
      SQLite.execute
        conn
        [sql|insert into feed_attachments (postId, filename, content) values (?,?,?)|]
        (postId, decodeUtf8 $ fileName, content)

-- Fetch the filenames for a single feed post
fetchAttachmentFilenames ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  Id ->
  m [Text]
fetchAttachmentFilenames conn postId = do
  (rows :: [[Text]]) <-
    liftIO $
      SQLite.query
        conn
        [sql|select filename from feed_attachments where postid = ?|]
        [postId]

  pure $ concat rows

-- Fetch a single attachment with its content
fetchAttachmentWithContent ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  Id ->
  Text ->
  m (Maybe (Text, BS.ByteString))
fetchAttachmentWithContent conn postId filename = do
  rows <-
    liftIO $
      SQLite.query
        conn
        [sql|select filename, content from feed_attachments where postid = ? and filename = ?|]
        (postId, filename)

  case rows of
    [] -> return Nothing
    [v] -> return $ Just v
    r -> throwString $ "Unexpected rows for getting event attachment" <> show r

deleteAttachments ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  Id ->
  [Text] ->
  m ()
deleteAttachments conn postId filenames = do
  forM_ filenames $ \fileName -> do
    liftIO $
      SQLite.execute
        conn
        [sql|delete from feed_attachments where postid = ? and filename = ? |]
        (postId, fileName)
