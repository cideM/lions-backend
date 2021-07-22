module Events.Attachments.Actions
  ( Actions (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Events.Attachments.Temporary as Temporary
import qualified Events.Attachments.Saved as Saved
import GHC.Generics

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []

data Actions = Actions
  { actionsKeep :: [Saved.Attachment],
    actionsDelete :: [Saved.Attachment],
    actionsDontUpload :: [Temporary.Attachment],
    actionsUpload :: [Temporary.Attachment]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Actions where
  parseJSON = Aeson.genericParseJSON defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 7}

instance ToJSON Actions where
  toEncoding = genericToEncoding defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 7}
