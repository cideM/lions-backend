module Feed.DB where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Text (Text)
import qualified Data.Time as Time
import Data.ByteString (ByteString)
import qualified Database.SQLite.Simple as SQLite
import Feed.Message
import Prelude hiding (id)

fetchAttachmentWithContent ::
  (MonadThrow m, MonadIO m) =>
  SQLite.Connection ->
  Id ->
  Text ->
  m (Maybe (Text, Maybe ByteString))
fetchAttachmentWithContent _ _ _ = undefined

-- | Returns the MOST RECENT welcome message, if there is one
get ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  m (Maybe Message)
get (Id id) = do
  conn <- asks App.getDb
  rows <- liftIO $ SQLite.query conn "SELECT id, content, date FROM welcome_text WHERE id = ?" [id]
  case rows of
    [(mid, msg, createdAt) :: (Int, Text, Time.UTCTime)] ->
      return . Just $ Message (Id mid) msg createdAt
    [] -> return Nothing
    other -> return . throwString $ "unexpected result from DB for welcome message" <> show other

-- | Returns all welcome messages in chronological order
getAll ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  m [Message]
getAll = do
  conn <- asks App.getDb
  rows <- liftIO $ SQLite.query_ conn "SELECT id, content, date FROM welcome_text ORDER BY date DESC"
  case rows of
    (msgs :: [(Int, Text, Time.UTCTime)]) ->
      return $ map (\(id, msg, createdAt) -> Message (Id id) msg createdAt) msgs

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
  m ()
save msg date = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "INSERT INTO welcome_text (content, date) VALUES (?, ?)" (msg, date)

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
