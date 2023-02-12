module Feed.Message where

import Data.Aeson (ToJSON)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Prelude hiding (id)

newtype Id = Id Int
  deriving (ToJSON, Show, ToField, FromField) via Int

-- A single entry in the news feed. "content" is either Markdown Text, or HTML,
-- after the Markdown text was parsed. The date is normalized to UTC and I
-- always display it in the Berlin time zone. I don't bother with summer time.
data Message content time = Message
  { id :: Id,
    content :: content,
    date :: time
  }
  deriving (Show)
