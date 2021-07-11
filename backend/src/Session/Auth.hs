module Session.Auth
  ( isUserAdmin,
    getSessionFromAuth,
    getAuthFromVault,
  )
where

import qualified Data.Vault.Lazy as Vault
import Session.Types
import User.Types (Role (..), UserId (..))
import Prelude hiding (id, log)

getAuthFromVault :: Vault.Key ([Role], UserId) -> Vault.Vault -> Authentication
getAuthFromVault sessionDataVaultKey vault =
  case Vault.lookup sessionDataVaultKey vault of
    Nothing -> IsNotAuthenticated
    Just (roles, userid) ->
      IsAuthenticated $
        if Admin `elem` roles
          then IsAdmin . AdminUser $ UserSession userid roles
          else IsUser $ UserSession userid roles

isUserAdmin :: Authenticated -> Bool
isUserAdmin (IsAdmin _) = True
isUserAdmin _ = False

getSessionFromAuth :: Authenticated -> UserSession
getSessionFromAuth (IsAdmin (AdminUser session)) = session
getSessionFromAuth (IsUser session) = session
