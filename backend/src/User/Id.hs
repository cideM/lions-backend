module User.Id where

import Data.Aeson
  ( FromJSON (..),
    ToJSON,
    defaultOptions,
    genericToEncoding,
    toEncoding,
  )
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import GHC.Generics

newtype Id = Id Int
  deriving (Generic)
  deriving (Eq, Ord, FromField, ToField, Show) via Int

unId :: Id -> Int
unId (Id i) = i

instance ToJSON Id where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Id
