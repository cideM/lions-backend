module Events.Event
  ( 
    Event (..),
    Create (..),
    Id (..),
  )
where

import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Events.Reply (Reply)
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Events.Attachments.Saved as Saved
import GHC.Generics

data Create = Create
  { createTitle :: Text,
    createDate :: Time.UTCTime,
    createFamilyAllowed :: Bool,
    createDescription :: Text,
    createLocation :: Text,
    createFiles :: [Text]
  }
  deriving (Show, Generic)

instance ToJSON Create where
  toEncoding = genericToEncoding defaultOptions

data Event = Event
  { eventTitle :: Text,
    eventDate :: Time.UTCTime,
    eventFamilyAllowed :: Bool,
    eventDescription :: Text,
    eventLocation :: Text,
    eventReplies :: [Reply],
    eventAttachments :: [Saved.Attachment]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Event where
  toEncoding = genericToEncoding defaultOptions

newtype Id = Id Int
  deriving (Show)
  deriving (Ord) via Int
  deriving (Eq) via Int
