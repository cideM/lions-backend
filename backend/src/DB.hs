module DB (withConnection) where

import qualified Database.SQLite.Simple as SQLite

withConnection :: String -> (SQLite.Connection -> IO a) -> IO a
withConnection sqlitePath f = do
  SQLite.withConnection sqlitePath $ \conn -> do
    -- Without "= ON" this only reads the setting, so foreign keys were never
    -- enforced before. Enforcement is per connection.
    SQLite.execute_ conn "PRAGMA foreign_keys = ON"
    -- Litestream checkpoints take short write locks; wait instead of failing.
    SQLite.execute_ conn "PRAGMA busy_timeout = 5000"
    f conn
