module Events.Attachments.Temporary (Attachment (..), save) where

-- This module is for attachments that were submitted with a request and are
-- currently stored temporarily, either on disk on in memory. They are not yet
-- marked as saved in the database, nor moved to a permament location on disk.

import qualified App
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
import qualified Data.Char as Char
import qualified Data.String.Interpolate as Interpolate
import Data.Text (Text)
import qualified Events.Event.Id as Event
import GHC.Generics
import qualified System.Directory
import System.FilePath ((</>))

-- This is an attachment in temporary storage that was just uploaded. The file
-- path is something temporary and that's why file path and file name are kept
-- separately.
data Attachment = Attachment
  { attachmentFileName :: Text,
    attachmentFileContentType :: Text,
    attachmentFilePath :: FilePath
  }
  deriving (Show, Eq, Generic)

instance ToJSON Attachment where
  toEncoding = genericToEncoding defaultOptions {A.fieldLabelModifier = lower1 . drop 10}

instance FromJSON Attachment where
  parseJSON = A.genericParseJSON defaultOptions {A.fieldLabelModifier = lower1 . drop 10}

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []

save ::
  ( MonadIO m,
    App.HasEventStorage env,
    MonadReader env m
  ) =>
  Event.Id ->
  FilePath ->
  FilePath ->
  m ()
save (Event.Id eid) source destinationFileName = do
  destinationDir <- asks App.getStorageDir
  let destDir = destinationDir </> [Interpolate.i|#{eid}|]
  liftIO $ System.Directory.createDirectoryIfMissing True destDir

  let dest = destDir </> destinationFileName

  liftIO $ System.Directory.copyFile source dest
