module Events.Attachments.Saved
  ( unFileName,
    FileName (..),
    remove,
    removeAll,
  )
where

import qualified App
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Text (Text)
import GHC.Generics
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Events.Event.Id as Event
import qualified System.Directory
import System.FilePath ((</>))

-- An attachment that's stored permanently is just a string representing a file
-- name in the database and a file with that same name on disk.
newtype FileName = FileName Text
  deriving (Show, Eq, Generic)

instance ToJSON FileName where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FileName

unFileName :: FileName -> Text
unFileName (FileName s) = s

removeAll ::
  ( MonadIO m,
    App.HasEventStorage env,
    MonadReader env m
  ) =>
  Event.Id ->
  m ()
removeAll (Event.Id eid) = do
  destinationDir <- asks App.getStorageDir
  liftIO $ System.Directory.removeDirectoryRecursive $ destinationDir </> show eid

remove ::
  ( MonadIO m,
    App.HasEventStorage env,
    MonadReader env m
  ) =>
  Event.Id ->
  FilePath ->
  m ()
remove (Event.Id eid) filename = do
  destinationDir <- asks App.getStorageDir
  liftIO $ System.Directory.removeFile $ destinationDir </> show eid </> filename
