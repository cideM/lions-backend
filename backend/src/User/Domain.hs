module User.Domain
  ( UserProfile (..),
    UserProfileCreate (..),
    UserEmail (..),
    showEmail,
    UserId (..),
    Role (..),
    isAdmin,
    isPassive,
    isBoard,
    isPresident,
    parseRole,
  )
where

import Data.Aeson (ToJSON, Value (..), defaultOptions, genericToEncoding, toEncoding, toJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Text.Email.Validate (EmailAddress, toByteString)

newtype UserEmail = UserEmail EmailAddress deriving (Show, Eq)

instance ToJSON UserEmail where
  toJSON (UserEmail email) = String $ showEmail email

data UserProfileCreate = UserProfileCreate
  { userCreateEmail :: UserEmail,
    userCreateFirstName :: Maybe Text,
    userCreateLastName :: Maybe Text,
    userCreateAddress :: Maybe Text,
    userCreateMobilePhoneNr :: Maybe Text,
    userCreateLandlineNr :: Maybe Text,
    userCreateBirthday :: Maybe Text,
    userCreateFirstNamePartner :: Maybe Text,
    userCreateLastNamePartner :: Maybe Text,
    userCreateBirthdayPartner :: Maybe Text,
    userCreateRoles :: NonEmpty Role
  }
  deriving (Show, Generic)

instance ToJSON UserProfileCreate where
  toEncoding = genericToEncoding defaultOptions

data UserProfile = UserProfile
  { userEmail :: UserEmail,
    userFirstName :: Maybe Text,
    userLastName :: Maybe Text,
    userAddress :: Maybe Text,
    userMobilePhoneNr :: Maybe Text,
    userLandlineNr :: Maybe Text,
    userBirthday :: Maybe Text,
    userFirstNamePartner :: Maybe Text,
    userLastNamePartner :: Maybe Text,
    userBirthdayPartner :: Maybe Text,
    userId :: UserId,
    userRoles :: NonEmpty Role
  }
  deriving (Show, Generic)

instance ToJSON UserProfile where
  toEncoding = genericToEncoding defaultOptions

data Role = Admin | User | Board | President | Passive deriving (Eq, Generic)

instance ToJSON Role where
  toEncoding = genericToEncoding defaultOptions

instance Show Role where
  show Admin = "admin"
  show User = "user"
  show Passive = "passive"
  show Board = "board"
  show President = "president"

isPassive :: Role -> Bool
isPassive Passive = True
isPassive _ = False

isAdmin :: Role -> Bool
isAdmin Admin = True
isAdmin _ = False

isBoard :: Role -> Bool
isBoard Board = True
isBoard _ = False

isPresident :: Role -> Bool
isPresident President = True
isPresident _ = False

parseRole :: Text -> Either Text Role
parseRole "admin" = Right Admin
parseRole "user" = Right User
parseRole "board" = Right Board
parseRole "president" = Right President
parseRole "passive" = Right Passive
parseRole v = Left $ "unknown role: " <> Text.pack (show v)

newtype UserId = UserId Int
  deriving (Show)
  deriving (Generic)
  deriving (Eq)

instance ToJSON UserId where
  toEncoding = genericToEncoding defaultOptions

showEmail :: EmailAddress -> Text
showEmail = decodeUtf8 . toByteString
