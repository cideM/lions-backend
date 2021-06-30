module DB (withConnection) where

import qualified Database.SQLite.Simple as SQLite

withConnection :: String -> (SQLite.Connection -> IO a) -> IO a
withConnection sqlitePath f = do
  SQLite.withConnection sqlitePath $ \conn -> do
    SQLite.execute_ conn "PRAGMA foreign_keys"
    f conn
