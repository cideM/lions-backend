module Session.DB where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Session.Types
import User.Types (UserId (..))

get ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  SessionId ->
  m (Maybe Session)
get (SessionId sid) = do
  conn <- asks App.getDb
  rows <- liftIO $ SQLite.query conn "SELECT key,expires,userid FROM sessions WHERE key = ?" [sid]
  case rows of
    [(key, expires, userid) :: (Text, Time.UTCTime, Int)] ->
      return . Just $ Session (SessionId key) expires (UserId userid)
    [] -> return Nothing
    other -> throwString $ "unexpected DB result for session: " <> show other
