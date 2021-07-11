module Request.Types (IdVaultKey) where

import Data.UUID (UUID)
import qualified Data.Vault.Lazy as Vault

type IdVaultKey = Vault.Key UUID
