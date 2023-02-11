module Feed.Message where

import Data.Aeson (ToJSON)
import qualified Data.Time as Time
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Prelude hiding (id)

newtype Id = Id Int
  deriving (ToJSON, Show, ToField, FromField) via Int

data Message content = Message Id content Time.UTCTime
  deriving (Show)
