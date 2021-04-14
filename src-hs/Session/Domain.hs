{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Session.Domain
  ( Session (..),
    SessionId (..),
    makeValidSession,
    ValidSession (..),
    SessionDataVaultKey,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import TextShow
import User.Domain (Role, UserId)

data Session = Session SessionId Time.UTCTime UserId deriving (Show)

type SessionDataVaultKey = Vault.Key ([Role], UserId)

newtype SessionId = SessionId Text
  deriving (Show)
  deriving (TextShow) via Text

newtype ValidSession = ValidSession Session deriving (Show)

makeValidSession :: Session -> IO (Either Text ValidSession)
makeValidSession s@(Session _ expires _) = do
  now <- Time.getCurrentTime
  return $
    if now >= expires
      then Left $ "session expired at: " <> Text.pack (show expires)
      else Right $ ValidSession s
