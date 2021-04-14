{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Session.DB (getSessionFromDb, saveSession) where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow (..))
import Session.Domain (Session (..), SessionId (..), ValidSession (..))
import User.Domain (UserId (..))
import Prelude hiding (id)

newtype DBSession = DBSession Session deriving (Show)

instance FromRow DBSession where
  fromRow = do
    key <- SQLite.field
    expires <- SQLite.field
    DBSession . Session (SessionId key) expires . UserId <$> SQLite.field

instance ToRow DBSession where
  toRow (DBSession (Session (SessionId id) expires (UserId userId))) = [toField id, toField expires, toField userId]

getSessionFromDb :: (MonadIO m) => SQLite.Connection -> SessionId -> m (Maybe Session)
getSessionFromDb conn (SessionId id) =
  liftIO $
    SQLite.query conn "SELECT key,expires,userid FROM sessions WHERE key = ?" [id]
      >>= \case
        [DBSession s] -> return $ Just s
        [] -> return Nothing
        other -> throwString $ "unexpected DB result: " <> show other

saveSession :: SQLite.Connection -> ValidSession -> IO ()
saveSession conn (ValidSession session) =
  SQLite.execute conn "INSERT INTO sessions (key,expires,userid) VALUES (?,?,?)" (DBSession session)
