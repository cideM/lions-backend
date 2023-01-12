{-# LANGUAGE DeriveAnyClass #-}

module Events.Reply where

import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField (ToField)
import GHC.Generics
import qualified User.Email as UserEmail
import qualified User.Id as User

-- Reply is information on RSVP'ing to an event
data Reply = Reply
  { replyComing :: ReplyComing,
    replyUserEmail :: UserEmail.Email,
    replyUserId :: User.Id,
    replyGuests :: ReplyGuests
  }
  deriving (Show, Generic, Eq, Ord)
  deriving (FromRow)

newtype ReplyComing = ReplyComing Bool
  deriving (Show, Eq, Ord, Generic)
  deriving (ToField, FromField) via Bool

unReplyComing :: ReplyComing -> Bool
unReplyComing (ReplyComing s) = s

parseReplyComingFromNumber :: Integer -> Either Text ReplyComing
parseReplyComingFromNumber 1 = Right $ ReplyComing True
parseReplyComingFromNumber 0 = Right $ ReplyComing False
parseReplyComingFromNumber n = Left $ "Invalid value for RSVP: " <> Text.pack (show n)

newtype ReplyGuests = ReplyGuests Integer
  deriving (Show, Eq, Ord, Generic)
  deriving (FromField, ToField) via Integer

unReplyGuests :: ReplyGuests -> Integer
unReplyGuests (ReplyGuests s) = s
