module Feed.Message
  ( Id (..),
    Message (..),
    get,
    getAll,
    delete,
    save,
    update,
    render,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Lucid
import qualified Network.URI as URI
import Prelude hiding (id)

newtype Id = Id Int deriving (Show)

data Message = Message Id Text Time.UTCTime deriving (Show)

-- | Returns the MOST RECENT welcome message, if there is one
get ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  m (Maybe Message)
get (Id id) = do
  conn <- asks App.getDb
  rows <- liftIO $ SQLite.query conn "SELECT id, content, date FROM welcome_text WHERE id = ?" [id]
  case rows of
    [(mid, msg, createdAt) :: (Int, Text, Time.UTCTime)] ->
      return . Just $ Message (Id mid) msg createdAt
    [] -> return Nothing
    other -> return . throwString $ "unexpected result from DB for welcome message" <> show other

-- | Returns all welcome messages in chronological order
getAll ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  m [Message]
getAll = do
  conn <- asks App.getDb
  rows <- liftIO $ SQLite.query_ conn "SELECT id, content, date FROM welcome_text ORDER BY date DESC"
  case rows of
    (msgs :: [(Int, Text, Time.UTCTime)]) ->
      return $ map (\(id, msg, createdAt) -> Message (Id id) msg createdAt) msgs

delete ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  m ()
delete (Id id) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "DELETE FROM welcome_text WHERE id = ?" [id]

save ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Text ->
  Time.UTCTime ->
  m ()
save msg date = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "INSERT INTO welcome_text (content, date) VALUES (?, ?)" (msg, date)

update ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  Text ->
  Time.UTCTime ->
  m ()
update (Id id) newMsg newDate = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "UPDATE welcome_text SET content = ?, date = ? WHERE id = ?" (newMsg, newDate, id)

-- render converts the plain text message into HTML and converts URLs into
-- actual anchor links. An empty line is used to generate <p> tags.
render :: Text -> Html ()
render text =
  mapM_ convertParagraph $ Text.splitOn "\r\n\r\n" text
  where
    convertParagraph :: Text -> Html ()
    convertParagraph content =
      p_ []
        . mconcat
        . List.intersperse (toHtml (" " :: Text) :: Html ())
        . map maybeInsertUrl
        $ Text.words content

    maybeInsertUrl word =
      -- This is an ugly hack and also I'm done with Haskell and this project
      -- and all of this crazy complexity. Haskell seems to have several
      -- competing URI libraries but all of them recognize foo: as a URI
      -- whereas I just want a URL parser that recognizes stuff like foo.com
      -- but not foo:
      case URI.parseURI (Text.unpack (Text.dropEnd 1 word)) of
        Just _ -> a_ [href_ word] $ toHtml word
        Nothing -> toHtml word
