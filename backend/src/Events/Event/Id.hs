module Events.Event.Id
  ( Id (..),
  )
where

newtype Id = Id Int
  deriving (Show)
  deriving (Ord) via Int
  deriving (Eq) via Int
