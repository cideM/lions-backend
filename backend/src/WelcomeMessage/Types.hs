module WelcomeMessage.Types where

import Data.Text (Text)
import qualified Data.Time as Time

newtype WelcomeMsgId = WelcomeMsgId Int deriving (Show)

data WelcomeMsg = WelcomeMsg WelcomeMsgId Text Time.UTCTime deriving (Show)

