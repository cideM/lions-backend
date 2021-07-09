module Request.Middleware (middleware) where

import Data.UUID.V4 (nextRandom)
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import Request.Types (IdVaultKey)

middleware :: IdVaultKey -> Wai.Application -> Wai.Application
middleware vaultKey next req send = do
  uuid <- nextRandom
  let vault' = Vault.insert vaultKey uuid (Wai.vault req)
      req' = req {Wai.vault = vault'}
  next req' send

