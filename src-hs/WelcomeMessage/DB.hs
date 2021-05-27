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
getWelcomeMsgFromDb :: SQLite.Connection -> WelcomeMsgId -> IO (Maybe WelcomeMsg)
getWelcomeMsgFromDb conn (WelcomeMsgId id) =
  (SQLite.query conn "SELECT id, content, date FROM welcome_text WHERE id = ?" [id])
    >>= \case
      [DBWelcomeMsg welcomeMsg] -> return $ Just welcomeMsg
      [] -> return Nothing
      other -> return . throwString $ "unexpected result from DB for welcome message" <> show other

-- | Returns all welcome messages in chronological order
getAllWelcomeMsgsFromDb :: SQLite.Connection -> IO [WelcomeMsg]
getAllWelcomeMsgsFromDb conn =
  (SQLite.query_ conn "SELECT id, content, date FROM welcome_text ORDER BY date DESC")
    >>= \case
      (msgs :: [DBWelcomeMsg]) -> return $ map (\(DBWelcomeMsg welcomeMsg) -> welcomeMsg) msgs

updateWelcomeMsg :: SQLite.Connection -> WelcomeMsgId -> Text -> IO ()
updateWelcomeMsg conn (WelcomeMsgId id) newMsg =
  SQLite.execute conn "UPDATE welcome_text SET content = ? WHERE id = ?" [newMsg, Text.pack $ show id]

saveNewWelcomeMsg :: SQLite.Connection -> Text -> IO ()
saveNewWelcomeMsg conn =
  SQLite.execute conn "INSERT INTO welcome_text (content, date) VALUES (?, datetime('now'))" . SQLite.Only

deleteMessage :: SQLite.Connection -> WelcomeMsgId -> IO ()
deleteMessage conn (WelcomeMsgId id) = SQLite.execute conn "DELETE FROM welcome_text WHERE id = ?" [id]
