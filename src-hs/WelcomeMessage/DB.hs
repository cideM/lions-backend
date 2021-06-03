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
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import WelcomeMessage.Domain (WelcomeMsg (..), WelcomeMsgId (..))
import Prelude hiding (id)

-- | Returns the MOST RECENT welcome message, if there is one
getWelcomeMsgFromDb :: SQLite.Connection -> WelcomeMsgId -> IO (Maybe WelcomeMsg)
getWelcomeMsgFromDb conn (WelcomeMsgId id) =
  (SQLite.query conn "SELECT id, content, date FROM welcome_text WHERE id = ?" [id])
    >>= \case
      [(mid, msg, createdAt) :: (Int, Text, Time.UTCTime)] ->
        return . Just $ WelcomeMsg (WelcomeMsgId mid) msg createdAt
      [] -> return Nothing
      other -> return . throwString $ "unexpected result from DB for welcome message" <> show other

-- | Returns all welcome messages in chronological order
getAllWelcomeMsgsFromDb :: SQLite.Connection -> IO [WelcomeMsg]
getAllWelcomeMsgsFromDb conn =
  (SQLite.query_ conn "SELECT id, content, date FROM welcome_text ORDER BY date DESC")
    >>= \case
      (msgs :: [(Int, Text, Time.UTCTime)]) ->
        return $ map (\(id, msg, createdAt) -> WelcomeMsg (WelcomeMsgId id) msg createdAt) msgs

updateWelcomeMsg :: SQLite.Connection -> WelcomeMsgId -> Text -> IO ()
updateWelcomeMsg conn (WelcomeMsgId id) newMsg =
  SQLite.execute conn "UPDATE welcome_text SET content = ? WHERE id = ?" [newMsg, Text.pack $ show id]

saveNewWelcomeMsg :: SQLite.Connection -> Text -> IO ()
saveNewWelcomeMsg conn =
  SQLite.execute conn "INSERT INTO welcome_text (content, date) VALUES (?, datetime('now'))" . SQLite.Only

deleteMessage :: SQLite.Connection -> WelcomeMsgId -> IO ()
deleteMessage conn (WelcomeMsgId id) = SQLite.execute conn "DELETE FROM welcome_text WHERE id = ?" [id]
