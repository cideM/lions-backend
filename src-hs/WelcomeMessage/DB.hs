{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WelcomeMessage.DB
  ( getWelcomeMsgFromDb,
    getAllWelcomeMsgsFromDb,
    deleteMessage,
    updateWelcomeMsg,
    saveNewWelcomeMsg,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow (FromRow)
import WelcomeMessage.Domain (WelcomeMsg (..), WelcomeMsgId (..))
import Prelude hiding (id)

newtype DBWelcomeMsg = DBWelcomeMsg WelcomeMsg deriving (Show)

instance FromRow DBWelcomeMsg where
  fromRow = do
    id <- SQLite.field
    content <- SQLite.field
    DBWelcomeMsg . WelcomeMsg (WelcomeMsgId id) content <$> SQLite.field

-- | Returns the MOST RECENT welcome message, if there is one
getWelcomeMsgFromDb :: (MonadIO m) => SQLite.Connection -> WelcomeMsgId -> m (Maybe WelcomeMsg)
getWelcomeMsgFromDb conn (WelcomeMsgId id) =
  liftIO (SQLite.query conn "SELECT id, content, date FROM welcome_text WHERE id = ?" [id])
    >>= \case
      [DBWelcomeMsg welcomeMsg] -> return $ Just welcomeMsg
      [] -> return Nothing
      other -> return . throwString $ "unexpected result from DB for password" <> show other

-- | Returns all welcome messages in chronological order
getAllWelcomeMsgsFromDb :: (MonadIO m) => SQLite.Connection -> m [WelcomeMsg]
getAllWelcomeMsgsFromDb conn =
  liftIO (SQLite.query_ conn "SELECT id, content, date FROM welcome_text ORDER BY date DESC")
    >>= \case
      (msgs :: [DBWelcomeMsg]) -> return $ map (\(DBWelcomeMsg welcomeMsg) -> welcomeMsg) msgs

updateWelcomeMsg :: SQLite.Connection -> WelcomeMsgId -> Text -> IO ()
updateWelcomeMsg conn (WelcomeMsgId id) newMsg =
  SQLite.execute conn "UPDATE welcome_text SET content = ? WHERE id = ?" [newMsg, Text.pack $ show id]

saveNewWelcomeMsg :: SQLite.Connection -> Text -> IO ()
saveNewWelcomeMsg conn =
  SQLite.execute conn "INSERT INTO welcome_text (content, date) VALUES (?, datetime('now'))" . SQLite.Only

deleteMessage :: (MonadIO m) => SQLite.Connection -> WelcomeMsgId -> m ()
deleteMessage conn (WelcomeMsgId id) = do
  liftIO $ SQLite.execute conn "DELETE FROM welcome_text WHERE id = ?" [id]
