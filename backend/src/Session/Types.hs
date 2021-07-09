module Session.Types
  ( VaultKey,
    UserSession (..),
    Authentication (..),
    AdminUser (..),
    Authenticated (..),
    SessionDataVaultKey,
    Session (..),
    SessionId (..),
    ValidSession (..),
  )
where

import Data.Text (Text)
import qualified Data.Time as Time
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

data Session = Session SessionId Time.UTCTime UserId deriving (Show)

type SessionDataVaultKey = Vault.Key ([Role], UserId)

newtype SessionId = SessionId Text
  deriving (Show, Eq)

-- A wrapper around a potentially invalid session so that I can differentiate
-- the two possible session types through types. I should not export the constructor actually.
-- TODO: Hide constructor
newtype ValidSession = ValidSession Session deriving (Show)
