{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PasswordReset.Domain
  ( TokenId (..),
    Token (..),
    TokenCreate (..),
    -- Don't export constructor
    Hashed,
    unhash,
    hashPw,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BCrypt
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import GHC.Generics
import User.Domain (UserId)

newtype TokenId = TokenId Int
  deriving (Show, Eq)
  deriving (Generic)

instance ToJSON TokenId where
  toEncoding = genericToEncoding defaultOptions

data TokenCreate = TokenCreate
  { tokenCreateValue :: Text,
    tokenCreateExpires :: Time.UTCTime,
    tokenCreateUserId :: UserId
  }
  deriving (Show, Eq, Generic)

data Token = Token
  { tokenValue :: Text,
    tokenExpires :: Time.UTCTime,
    tokenId :: TokenId,
    tokenUserId :: UserId
  }
  deriving (Show, Eq, Generic)

instance ToJSON Token where
  toEncoding = genericToEncoding defaultOptions

newtype Hashed = Hashed Text deriving (Show)

hashPw :: (MonadIO m, MonadThrow m) => ByteString -> m Hashed
hashPw unhashed =
  liftIO $
    BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy unhashed >>= \case
      Nothing -> throwString "hashing password failed"
      Just pw' -> return . Hashed $ decodeUtf8 pw'

unhash :: Hashed -> Text
unhash (Hashed s) = s
