module Events.Reply
  ( Reply (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import GHC.Generics
import User.Types (UserEmail (..), UserId (..))

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

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []
