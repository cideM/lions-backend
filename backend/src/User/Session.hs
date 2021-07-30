module User.Session
  ( Session (..),
    isAdmin,
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
import qualified User.Id
import qualified User.Id as User
import User.Role.Role (Role)
import qualified User.Role.Role as User

-- There's some overlap with the Session type defined in Session.Session. That
-- type represents the session data retrieved from the DB based on the ID
-- stored client side. This Session here, on the other hand, is the user info
-- associated with that session. I think it's fine to separate them, but
-- Session has a confusing name. It's closer to a special type to marshal and
-- unmarshal to and from a database. This Sessino is closer to the actual
-- (user) session, at least in spirit.
-- Also, this type is called Session because it's expected to be used as a
--
-- qualified import, as in User.Session
data Session = Session
  { sessionUserId :: User.Id.Id,
    sessionUserRoles :: [Role]
  }
  deriving (Show, Eq)

type VaultKey = Vault.Key ([User.Role], User.Id)

-- These aren't exported on purpose so I don't have nested destructuring
-- everywhere that would be tedious to refactor in case I ever figure out a
-- better way of modeling this.
data Authentication = IsNotAuthenticated | IsAuthenticated Authenticated deriving (Show, Eq)

data Authenticated = IsUser Session | IsAdmin Admin deriving (Show, Eq)

newtype Admin = Admin Session deriving (Show, Eq)

get :: Authentication -> Maybe Session
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
          then IsAdmin . Admin $ Session userid roles
          else IsUser $ Session userid roles

isAdmin' :: Authenticated -> Bool
isAdmin' (IsAdmin _) = True
isAdmin' _ = False

get' :: Authenticated -> Session
get' (IsAdmin (Admin session)) = session
get' (IsUser session) = session
