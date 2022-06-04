module User.Id (Id (..)) where

import Data.Aeson
  ( FromJSON (..),
    ToJSON,
    defaultOptions,
    genericToEncoding,
    toEncoding,
  )
import GHC.Generics

newtype Id = Id Int
  deriving (Show)
  deriving (Generic)
  deriving (Eq)
  deriving (Ord)

instance ToJSON Id where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Id
