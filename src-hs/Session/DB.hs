module Session.DB
  ( getSessionsFromDbByUser,
    getSessionFromDb,
    saveSession,
    deleteSession,
  )
where

import Control.Exception.Safe
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Session.Domain (Session (..), SessionId (..), ValidSession (..))
import User.Domain (UserId (..))
import Prelude hiding (id)

getSessionFromDb :: SQLite.Connection -> SessionId -> IO (Maybe Session)
getSessionFromDb conn (SessionId id) =
  SQLite.query conn "SELECT key,expires,userid FROM sessions WHERE key = ?" [id]
    >>= \case
      [(key, expires, userid) :: (Text, Time.UTCTime, Int)] ->
        return . Just $ Session (SessionId key) expires (UserId userid)
      [] -> return Nothing
      other -> throwString $ "unexpected DB result for session: " <> show other

getSessionsFromDbByUser :: SQLite.Connection -> UserId -> IO [Session]
getSessionsFromDbByUser conn (UserId id) =
  SQLite.query conn "SELECT key,expires,userid FROM sessions WHERE userid = ?" [id]
    >>= \case
      (sessions :: [(Text, Time.UTCTime, Int)]) ->
        return $ map (\(key, expires, userid) -> Session (SessionId key) expires (UserId userid)) sessions

saveSession :: SQLite.Connection -> ValidSession -> IO ()
saveSession conn (ValidSession (Session (SessionId key) expires (UserId uid))) =
  SQLite.execute conn "INSERT INTO sessions (key,expires,userid) VALUES (?,?,?)" (key, expires, uid)

deleteSession :: SQLite.Connection -> Session -> IO ()
deleteSession conn (Session (SessionId id) _ _) =
  SQLite.execute conn "DELETE FROM sessions WHERE key = ?" $ SQLite.Only id
