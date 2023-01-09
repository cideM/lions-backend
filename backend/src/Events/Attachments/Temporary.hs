module Events.Attachments.Temporary (Attachment (..)) where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
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
  toEncoding = genericToEncoding defaultOptions {A.fieldLabelModifier = lower1 . drop 10}

instance FromJSON Attachment where
  parseJSON = A.genericParseJSON defaultOptions {A.fieldLabelModifier = lower1 . drop 10}

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []
