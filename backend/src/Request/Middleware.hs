module Request.Middleware (middleware) where

import Data.UUID.V4 (nextRandom)
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Wai.Class as Wai
import Request.Types (IdVaultKey)

middleware :: (MonadIO m) => IdVaultKey -> Wai.MiddlewareT m
middleware vaultKey next req send = do
  uuid <- liftIO $ nextRandom
  let vault' = Vault.insert vaultKey uuid (Wai.vault req)
      req' = req {Wai.vault = vault'}
  next req' send
