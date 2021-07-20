module Session.Types
  ( VaultKey,
    UserSession (..),
    Authentication (..),
    AdminUser (..),
    Authenticated (..),
    SessionDataVaultKey
  )
where

import qualified Data.Vault.Lazy as Vault
import User.Types (Role (..), UserId (..))

type VaultKey = Vault.Key ([Role], UserId)

data UserSession = UserSession
  { userSessionUserId :: UserId,
    userSessionUserRoles :: [Role]
  }
  deriving (Show, Eq)

data Authentication = IsNotAuthenticated | IsAuthenticated Authenticated deriving (Show, Eq)

data Authenticated = IsUser UserSession | IsAdmin AdminUser deriving (Show, Eq)

newtype AdminUser = AdminUser UserSession deriving (Show, Eq)

type SessionDataVaultKey = Vault.Key ([Role], UserId)
