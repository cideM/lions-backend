module WelcomeMessage.Domain (WelcomeMsg (..)) where

import Data.Text (Text)
import qualified Data.Time as Time

data WelcomeMsg = WelcomeMsg Text Time.UTCTime deriving (Show)
