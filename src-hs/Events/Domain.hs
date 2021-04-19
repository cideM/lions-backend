{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.Domain
  ( Reply (..),
    Event (..),
    EventCreate (..),
    EventId (..),
  )
where

import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Text (Text)
import qualified Data.Time as Time
import GHC.Generics
import User.Domain (UserEmail (..), UserId (..))

data Reply = Reply
  { replyComing :: Bool,
    replyEmail :: UserEmail,
    replyUserId :: UserId,
    replyGuests :: Int
  }
  deriving (Show, Generic)

instance ToJSON Reply where
  toEncoding = genericToEncoding defaultOptions

-- TODO: Ok this duplication also sucks... maybe tuples again? :(
data EventCreate = EventCreate
  { eventCreateTitle :: Text,
    eventCreateDate :: Time.UTCTime,
    eventCreateFamilyAllowed :: Bool,
    eventCreateDescription :: Text,
    eventCreateLocation :: Text
  }
  deriving (Show, Generic)

instance ToJSON EventCreate where
  toEncoding = genericToEncoding defaultOptions

data Event = Event
  { eventTitle :: Text,
    eventDate :: Time.UTCTime,
    eventFamilyAllowed :: Bool,
    eventDescription :: Text,
    eventLocation :: Text,
    eventReplies :: [Reply]
  }
  deriving (Show, Generic)

instance ToJSON Event where
  toEncoding = genericToEncoding defaultOptions

newtype EventId = EventId Int
  deriving (Show)
  deriving (Ord) via Int
  deriving (Eq) via Int
