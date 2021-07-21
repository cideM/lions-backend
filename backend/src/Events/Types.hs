module Events.Types
  ( Reply (..),
    Event (..),
    Create (..),
    Id (..),
    Attachment (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Time as Time
import GHC.Generics
import User.Types (UserEmail (..), UserId (..))

-- This is an attachment that's already stored in the database. I didn't store
-- the content type together with the files and the file name is in this case
-- equal to the file path.
data Attachment = Attachment {attachmentFileName :: Text} deriving (Show, Generic, Eq)

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []

instance FromJSON Attachment where
  parseJSON = Aeson.genericParseJSON defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 10}

instance ToJSON Attachment where
  toEncoding = genericToEncoding defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 10}

-- Now that I'm writing tests that involve this data type I find it a bit weird
-- that it doesn't have the event ID. Strictly speaking a reply is meaningless
-- without its event ID. TODO: Reconsider if this should carry its ID around!
data Reply = Reply
  { replyComing :: Bool,
    replyUserEmail :: UserEmail,
    replyUserId :: UserId,
    replyGuests :: Int
  }
  deriving (Show, Generic, Eq)

instance FromJSON Reply where
  parseJSON = Aeson.genericParseJSON defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 5}

instance ToJSON Reply where
  toEncoding = genericToEncoding defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 5}

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
    eventAttachments :: [Attachment]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Event where
  toEncoding = genericToEncoding defaultOptions

newtype Id = Id Int
  deriving (Show)
  deriving (Ord) via Int
  deriving (Eq) via Int
