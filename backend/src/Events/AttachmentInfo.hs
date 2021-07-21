module Events.AttachmentInfo (AttachmentInfo(..)) where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Text (Text)
import GHC.Generics

-- This is an attachment in temporary storage that was just uploaded. The file
-- path is something temporary and that's why file path and file name are kept
-- separately.
data AttachmentInfo = AttachmentInfo
  { attachmentInfoFileName :: Text,
    attachmentInfoFileContentType :: Text,
    attachmentInfoFilePath :: FilePath
  }
  deriving (Show, Eq, Generic)

instance ToJSON AttachmentInfo where
  toEncoding = genericToEncoding defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 14}

instance FromJSON AttachmentInfo where
  parseJSON = Aeson.genericParseJSON defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 14}

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []

