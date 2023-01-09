module Events.Attachments.Saved (unFileName, FileName (..)) where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Text (Text)
import GHC.Generics

-- An attachment that's stored permanently is just a string representing a file
-- name in the database and a file with that same name on disk.
newtype FileName = FileName Text
  deriving (Show, Eq, Generic, Ord)

instance ToJSON FileName where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FileName

unFileName :: FileName -> Text
unFileName (FileName s) = s
