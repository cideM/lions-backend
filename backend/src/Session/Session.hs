module Session.Session
  ( Session (..),
    Id (..),
    deleteUser,
    get
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import qualified User.Id as User

newtype Id = Id Text
  deriving (Show, Eq)

-- TODO: It shouldn't be possible to use this type without checking if it's expired
data Session = Session Id Time.UTCTime User.Id deriving (Show)

-- Delete all sessions for the given User ID
deleteUser ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m
  ) =>
  User.Id ->
  m ()
deleteUser (User.Id uid) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "delete from sessions where userid = ?" [uid]

get ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  m (Maybe Session)
get (Id sid) = do
  conn <- asks App.getDb
  rows <- liftIO $ SQLite.query conn "SELECT key,expires,userid FROM sessions WHERE key = ?" [sid]
  case rows of
    [(key, expires, userid) :: (Text, Time.UTCTime, Int)] ->
      return . Just $ Session (Id key) expires (User.Id userid)
    [] -> return Nothing
    other -> throwString $ "unexpected DB result for session: " <> show other
