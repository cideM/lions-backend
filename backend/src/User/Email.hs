module User.Email where

import Data.Aeson
  ( FromJSON (..),
    ToJSON,
    Value (..),
    toJSON,
  )
import qualified Data.Aeson as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple (ResultError (..))
import Database.SQLite.Simple.FromField (FromField (..), returnError)
import Text.Email.Validate (EmailAddress, emailAddress, toByteString)

newtype Email = Email EmailAddress
  deriving (Eq, Ord)

-- The instance from the library will render an email with quotes around it
instance Show Email where
  show (Email s) = Text.unpack . decodeUtf8 $ toByteString s

unEmail :: Email -> EmailAddress
unEmail (Email v) = v

instance FromField Email where
  fromField f = do
    s :: Text <- fromField f
    case emailAddress (encodeUtf8 s) of
      Just addr -> return $ Email addr
      Nothing -> returnError ConversionFailed f "expecting an SQLText column type"

instance ToJSON Email where
  toJSON (Email email) = String . Text.pack $ show email

instance FromJSON Email where
  parseJSON (Aeson.String s) =
    case emailAddress (encodeUtf8 s) of
      Just addr -> return $ Email addr
      Nothing -> fail [i|couldn't parse email: #{s}|]
  parseJSON _ = fail "wrong type for email"
