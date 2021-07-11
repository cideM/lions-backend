module Request.Middleware (middleware, IdVaultKey) where

import qualified App
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import qualified Wai.Class as Wai

type IdVaultKey = Vault.Key UUID

middleware ::
  ( MonadIO m,
    App.HasRequestIdVaultKey env,
    MonadReader env m
  ) =>
  Wai.MiddlewareT m
middleware next req send = do
  vaultKey <- asks App.getRequestIdVaultKey
  uuid <- liftIO $ nextRandom
  let vault' = Vault.insert vaultKey uuid (Wai.vault req)
      req' = req {Wai.vault = vault'}
  next req' send
