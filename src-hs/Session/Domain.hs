{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Session.Domain
  ( Session (..),
    SessionId (..),
    makeValidSession,
    createNewSession,
    ValidSession (..),
    SessionDataVaultKey,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Session (genSessionId)
import Time.Time (timeDaysFromNow)
import User.Domain (Role, UserId)

data Session = Session SessionId Time.UTCTime UserId deriving (Show)

type SessionDataVaultKey = Vault.Key ([Role], UserId)

newtype SessionId = SessionId Text
  deriving (Show, Eq)

newtype ValidSession = ValidSession Session deriving (Show)

createNewSession :: UserId -> IO ValidSession
createNewSession uid = do
  expires <- timeDaysFromNow 30
  sessionid <- decodeUtf8 <$> genSessionId
  return . ValidSession $ Session (SessionId sessionid) expires uid

makeValidSession :: Session -> IO (Either Text ValidSession)
makeValidSession s@(Session _ expires _) = do
  now <- Time.getCurrentTime
  if now >= expires
    then return . Left $ "session expired at: " <> Text.pack (show expires)
    else return . pure $ ValidSession s
