module Events.Attachments.Temporary (Attachment(..)) where

-- This module is for attachments that were submitted with a request and are
-- currently stored temporarily, either on disk on in memory. They are not yet
-- marked as saved in the database, nor moved to a permament location on disk.

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Text (Text)
import GHC.Generics

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
  toEncoding = genericToEncoding defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 10}

instance FromJSON Attachment where
  parseJSON = Aeson.genericParseJSON defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 10}

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []

