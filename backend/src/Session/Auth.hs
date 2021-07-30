-- I can't really get both Session and User into a state where I'm happy with
-- the modules. This module holds functions and types for deciding what kind of
-- access a visitor has. The "User" that is mentioned here mostly referes to
-- the ID and user role pair that is stored in Vault. The full user profile
-- lives somewhere else entirely.
--
-- The full list of user roles is stored in the user profile module. There's
-- some redundancy between the profile and the session user records. For
-- example, they both have a userId field
--
-- I'm not exporting the constructors on purpose.
--
-- Functions marked with ' work on Authenticated, whereas functions without '
-- work on Authentication
module Session.Auth
  ( isAdmin,
    isAdmin',
    get,
    getAuth,
    get',
    getAdmin,
    isAuthenticated,
    notAuthenticated,
    Authentication,
    Admin,
    Authenticated,
    VaultKey,
    fromVault,
  )
where

import Data.Maybe (isJust)
import qualified Data.Vault.Lazy as Vault
import qualified User.Id as User
import qualified User.Role.Role as User
import qualified User.Session as User

type VaultKey = Vault.Key ([User.Role], User.Id)

-- These aren't exported on purpose so I don't have nested destructuring
-- everywhere that would be tedious to refactor in case I ever figure out a
-- better way of modeling this.
data Authentication = IsNotAuthenticated | IsAuthenticated Authenticated deriving (Show, Eq)

data Authenticated = IsUser User.Session | IsAdmin Admin deriving (Show, Eq)

newtype Admin = Admin User.Session deriving (Show, Eq)

get :: Authentication -> Maybe User.Session
get = fmap get' . getAuth

isAdmin :: Authentication -> Bool
isAdmin = isJust . getAdmin

getAdmin :: Authentication -> Maybe Admin
getAdmin auth =
  getAuth auth >>= \case
    IsAdmin v -> Just v
    _ -> Nothing

getAuth :: Authentication -> Maybe Authenticated
getAuth (IsAuthenticated auth) = Just auth
getAuth _ = Nothing

isAuthenticated :: Authentication -> Bool
isAuthenticated = isJust . get

notAuthenticated :: Authentication
notAuthenticated = IsNotAuthenticated

fromVault :: VaultKey -> Vault.Vault -> Authentication
fromVault sessionDataVaultKey vault =
  case Vault.lookup sessionDataVaultKey vault of
    Nothing -> IsNotAuthenticated
    Just (roles, userid) ->
      IsAuthenticated $
        if User.Admin `elem` roles
          then IsAdmin . Admin $ User.Session userid roles
          else IsUser $ User.Session userid roles

isAdmin' :: Authenticated -> Bool
isAdmin' (IsAdmin _) = True
isAdmin' _ = False

get' :: Authenticated -> User.Session
get' (IsAdmin (Admin session)) = session
get' (IsUser session) = session
