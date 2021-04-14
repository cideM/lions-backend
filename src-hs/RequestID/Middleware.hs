{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RequestID.Middleware (middleware, RequestIdVaultKey) where

import Capability.Reader (HasReader (..), ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai

type WaiApp m = Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived

type RequestIdVaultKey = Vault.Key UUID

middleware ::
  ( MonadIO m,
    HasReader "requestIdVaultKey" RequestIdVaultKey m
  ) =>
  WaiApp m ->
  WaiApp m
middleware next req send = do
  requestIdVaultKey <- ask @"requestIdVaultKey"
  uuid <- liftIO nextRandom
  let vault' = Vault.insert requestIdVaultKey uuid (Wai.vault req)
      req' = req {Wai.vault = vault'}
  next req' send
