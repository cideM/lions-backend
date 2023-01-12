module User.Id (Id (..)) where

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
  deriving (Show, Generic, Eq, Ord)
  deriving (FromField, ToField) via Int

instance ToJSON Id where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Id
