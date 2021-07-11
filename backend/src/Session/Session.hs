module Session.Session
  ( isUserAdmin,
    getSessionFromAuth,
    getAuthFromVault,
    create,
    deleteUser,
    save,
    module Session.Types,
    module Session.Middleware,
    module Session.DB,
  )
where

import qualified App
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Network.Wai.Session (genSessionId)
import Session.DB
import Session.Middleware
import Session.Types
import Time (timeDaysFromNow)
import User.Types (Role (..), UserId (..))
import Prelude hiding (id, log)

create :: (MonadIO m) => UserId -> m ValidSession
create uid = do
  expires <- liftIO $ timeDaysFromNow 30
  sessionid <- liftIO $ decodeUtf8 <$> genSessionId
  return . ValidSession $ Session (SessionId sessionid) expires uid

-- Delete all sessions for the given User ID
deleteUser ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m
  ) =>
  UserId ->
  m ()
deleteUser (UserId uid) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "delete from sessions where userid = ?" [uid]

save ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m
  ) =>
  ValidSession ->
  m ()
save (ValidSession (Session (SessionId key) expires (UserId uid))) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "INSERT INTO sessions (key,expires,userid) VALUES (?,?,?)" (key, expires, uid)

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
