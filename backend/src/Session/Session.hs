module Session.Session
  ( Session (..),
    Id (..),
    deleteUser,
    get
  )
where

import qualified App
import Control.Monad
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
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
    MonadPlus m,
    MonadReader env m
  ) =>
  Id ->
  m Session
get (Id sid) = do
  conn <- asks App.getDb
  rows <- liftIO $ SQLite.query conn "SELECT key,expires,userid FROM sessions WHERE key = ?" [sid]
  case rows of
    [(key, expires, userid) :: (Text, Time.UTCTime, Int)] ->
      return $ Session (Id key) expires (User.Id userid)
    [] -> mzero
    other -> throwString $ "unexpected DB result for session: " <> show other
