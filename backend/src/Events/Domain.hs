module Events.Domain
  ( Reply (..),
    Event (..),
    EventCreate (..),
    EventAttachment (..),
    EventId (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Time as Time
import GHC.Generics
import User.Types (UserEmail (..), UserId (..))

-- Now that I'm writing tests that involve this data type I find it a bit weird
-- that it doesn't have the event ID. Strictly speaking a reply is meaningless
-- without its event ID. TODO: Reconsider if this should carry its ID around!
data Reply = Reply
  { replyComing :: Bool,
    replyEmail :: UserEmail,
    replyUserId :: UserId,
    replyGuests :: Int
  }
  deriving (Show, Generic, Eq)

instance ToJSON Reply where
  toEncoding = genericToEncoding defaultOptions

data EventCreate = EventCreate
  { eventCreateTitle :: Text,
    eventCreateDate :: Time.UTCTime,
    eventCreateFamilyAllowed :: Bool,
    eventCreateDescription :: Text,
    eventCreateLocation :: Text,
    eventCreateFiles :: [Text]
  }
  deriving (Show, Generic)

instance ToJSON EventCreate where
  toEncoding = genericToEncoding defaultOptions

data EventAttachment = EventAttachment {eventAttachmentFileName :: Text} deriving (Show, Generic, Eq)

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []

instance FromJSON EventAttachment where
  parseJSON = Aeson.genericParseJSON defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 15}

instance ToJSON EventAttachment where
  toEncoding = genericToEncoding defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 15}

data Event = Event
  { eventTitle :: Text,
    eventDate :: Time.UTCTime,
    eventFamilyAllowed :: Bool,
    eventDescription :: Text,
    eventLocation :: Text,
    eventReplies :: [Reply],
    eventAttachments :: [EventAttachment]
  }
  deriving (Show, Eq, Generic)

instance Semigroup Event where
  e1 <> e2 =
    e1
      { eventReplies = nub $ eventReplies e1 ++ eventReplies e2
      }

instance ToJSON Event where
  toEncoding = genericToEncoding defaultOptions

newtype EventId = EventId Int
  deriving (Show)
  deriving (Ord) via Int
  deriving (Eq) via Int
