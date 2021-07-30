module User.Role.Role
  ( Role (..),
    parse,
  )
where

-- I had to split this up because it's the only way to avoid import cycles.
-- The "get" and "save" functions need to import App, for the Has* constraint.
-- But App also needs to import Session.Auth, to define one of the Has*
-- classes.  That module imports from User.Role, causing an import cycle.

import Data.Aeson
  ( ToJSON,
    defaultOptions,
    genericToEncoding,
    toEncoding,
  )
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

data Role = Admin | User | Board | President | Passive deriving (Eq, Generic)

instance ToJSON Role where
  toEncoding = genericToEncoding defaultOptions

instance Show Role where
  show Admin = "admin"
  show User = "user"
  show Passive = "passive"
  show Board = "board"
  show President = "president"

parse :: Text -> Either Text Role
parse "admin" = Right Admin
parse "user" = Right User
parse "board" = Right Board
parse "president" = Right President
parse "passive" = Right Passive
parse v = Left $ "unknown role: " <> Text.pack (show v)
