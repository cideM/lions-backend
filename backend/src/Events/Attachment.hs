module Events.Attachment (Attachment (..)) where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Text (Text)
import GHC.Generics

-- This is an attachment that's already stored in the database. I didn't store
-- the content type together with the files and the file name is in this case
-- equal to the file path.
data Attachment = Attachment {attachmentFileName :: Text} deriving (Show, Generic, Eq)

instance FromJSON Attachment where
  parseJSON = Aeson.genericParseJSON defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 10}

instance ToJSON Attachment where
  toEncoding = genericToEncoding defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 10}

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []
