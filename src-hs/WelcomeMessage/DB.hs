{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WelcomeMessage.DB (getWelcomeMsgFromDb, saveNewWelcomeMsg) where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow (FromRow)
import WelcomeMessage.Domain (WelcomeMsg (..))
import Prelude hiding (id)

newtype DBWelcomeMsg = DBWelcomeMsg WelcomeMsg deriving (Show)

instance FromRow DBWelcomeMsg where
  fromRow = do
    content <- SQLite.field
    DBWelcomeMsg . WelcomeMsg content <$> SQLite.field

-- | getWelcomeMsgFromDb returns the MOST RECENT welcome message, if there is one
getWelcomeMsgFromDb :: (MonadIO m) => SQLite.Connection -> m (Maybe WelcomeMsg)
getWelcomeMsgFromDb conn =
  liftIO (SQLite.query_ conn "SELECT content, date FROM welcome_text ORDER BY date DESC LIMIT 1")
    >>= \case
      [DBWelcomeMsg welcomeMsg] -> return $ Just welcomeMsg
      [] -> return Nothing
      other -> return . throwString $ "unexpected result from DB for password" <> show other

saveNewWelcomeMsg :: SQLite.Connection -> Text -> IO ()
saveNewWelcomeMsg conn =
  SQLite.execute conn "INSERT INTO welcome_text (content, date) VALUES (?, datetime('now'))" . SQLite.Only
