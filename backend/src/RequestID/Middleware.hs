module RequestID.Middleware (middleware, RequestIdVaultKey) where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai

type RequestIdVaultKey = Vault.Key UUID

middleware ::  RequestIdVaultKey -> Wai.Application -> Wai.Application
middleware requestIdVaultKey next req send = do
  uuid <- nextRandom
  let vault' = Vault.insert requestIdVaultKey uuid (Wai.vault req)
      req' = req {Wai.vault = vault'}
  next req' send
