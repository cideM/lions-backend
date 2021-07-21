module Events.FileActions
  ( FileActions (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Events.Types (Attachment, AttachmentInfo)
import GHC.Generics

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []

data FileActions = FileActions
  { fileActionsKeep :: [Attachment],
    fileActionsDelete :: [Attachment],
    fileActionsDontUpload :: [AttachmentInfo],
    fileActionsUpload :: [AttachmentInfo]
  }
  deriving (Show, Eq, Generic)

instance FromJSON FileActions where
  parseJSON = Aeson.genericParseJSON defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 11}

instance ToJSON FileActions where
  toEncoding = genericToEncoding defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 11}
