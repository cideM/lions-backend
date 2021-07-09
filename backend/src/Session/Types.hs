module Session.Types (VaultKey) where

import qualified Data.Vault.Lazy as Vault
import User.Types (Role (..), UserId (..))

type VaultKey = Vault.Key ([Role], UserId)

