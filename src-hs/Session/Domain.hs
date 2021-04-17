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

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import TextShow
import User.Domain (Role, UserId)

data Session = Session SessionId Time.UTCTime UserId deriving (Show)

type SessionDataVaultKey = Vault.Key ([Role], UserId)

newtype SessionId = SessionId Text
  deriving (Show, Eq)
  deriving (TextShow) via Text

newtype ValidSession = ValidSession Session deriving (Show)

makeValidSession :: (MonadIO m, MonadError Text m) => Session -> m ValidSession
makeValidSession s@(Session _ expires _) = do
  now <- liftIO Time.getCurrentTime
  if now >= expires
    then throwError $ "session expired at: " <> Text.pack (show expires)
    else pure $ ValidSession s
