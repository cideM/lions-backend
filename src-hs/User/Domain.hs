{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module User.Domain
  ( UserProfile (..),
    showEmail,
    UserId (..),
    Role (..),
    isAdmin,
    isBoard,
    isPresident,
    parseRole,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import Text.Email.Validate (EmailAddress, toByteString)
import TextShow

data UserProfile = UserProfile
  { userEmail :: EmailAddress,
    userFirstName :: Maybe Text,
    userLastName :: Maybe Text,
    userAddress :: Maybe Text,
    userMobilePhoneNr :: Maybe Text,
    userLandlineNr :: Maybe Text,
    userBirthday :: Maybe Time.Day,
    userFirstNamePartner :: Maybe Text,
    userLastNamePartner :: Maybe Text,
    userBirthdayPartner :: Maybe Time.Day
  }
  deriving (Show)

data Role = Admin | User | Board | President deriving (Eq)

instance Show Role where
  show Admin = "admin"
  show User = "user"
  show Board = "board"
  show President = "president"

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
parseRole v = Left $ "unknown role: " <> Text.pack (show v)

newtype UserId = UserId Int
  deriving (Show)
  deriving (Eq)
  deriving (TextShow) via Int

showEmail :: EmailAddress -> Text
showEmail = decodeUtf8 . toByteString
