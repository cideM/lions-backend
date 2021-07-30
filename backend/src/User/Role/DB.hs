module User.Role.DB
  ( get,
    save,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Text (Text)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import Prelude hiding (id)
import qualified User.Id as User
import qualified User.Role.Role as Role

get ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  User.Id ->
  m (Maybe [Role.Role])
get (User.Id userId) = do
  conn <- asks App.getDb
  let q =
        [sql|
           WITH roles_for_id AS (SELECT roleid FROM user_roles WHERE userid = ?)
           SELECT label FROM roles_for_id JOIN roles ON id = roleid
        |]
  rows <- liftIO $ SQLite.query conn q [userId]
  case rows of
    [] -> return Nothing
    (roles :: [[Text]]) -> do
      case traverse Role.parse $ concat roles of
        Left e -> throwString $ "couldn't parse roles: " <> show e
        Right parsedRoles -> return $ Just parsedRoles

save ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  User.Id ->
  [Role.Role] ->
  m ()
save (User.Id userid) roles = do
  traverse getRoleId roles >>= \case
    [] -> throwString $ "no role IDs for roles: '" <> show roles <> "', shouldn't happen"
    (ids :: [Int]) ->
      forM_ ids insertRoleId
  where
    insertRoleId roleid = do
      conn <- asks App.getDb
      liftIO $ SQLite.execute conn "INSERT INTO user_roles (roleid, userid) VALUES (?,?)" (roleid, userid)
    getRoleId label = do
      conn <- asks App.getDb
      liftIO $
        SQLite.query conn "SELECT id FROM roles WHERE label = ?" [show label]
          >>= \case
            [] -> throwString $ "no role for label: " <> show label
            [SQLite.Only roleid] -> return roleid
            other -> throwString $ "unexpected DB result: " <> show other

