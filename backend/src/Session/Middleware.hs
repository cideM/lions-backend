module Session.Middleware (middleware) where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Katip as K
import Network.HTTP.Types (status302)
import qualified Network.Wai as Wai
import Session.Session (Session (..))
import qualified Session.Session as Session
import qualified Session.Valid
import qualified User.Role.DB as Role
import qualified Wai
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id, log)

-- This is the main user facing export. This middleware will try to log a user
-- in based on cookie information. The login information is then persisted in
-- the Vault associated with this particular request so that downstream code
-- can access it.
middleware ::
  ( MonadIO m,
    K.KatipContext m,
    App.HasSessionDataVaultKey env,
    App.HasDb env,
    MonadThrow m,
    App.HasSessionEncryptionKey env,
    MonadReader env m
  ) =>
  Wai.MiddlewareT m
middleware nextApp req send = do
  maybeRequestWithSession <- login req
  case maybeRequestWithSession of
    Nothing ->
      -- The user doesn't have a valid session. To prevent infinite redirects,
      -- we need to match on the route.
      case Wai.pathInfo req of
        ["login"] -> do
          nextApp req send
        ["passwort", "link"] -> do
          nextApp req send
        ["passwort", "aendern"] -> do
          nextApp req send
        _ -> send $ Wai.responseBuilder status302 [("Location", "/login")] ""
    Just req' -> nextApp req' send

-- login adds the user session to the vault, and adds the vault to the request,
-- in case the user has a valid session. If not, the original request is
-- returned unchanged.
login ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env,
    App.HasSessionEncryptionKey env,
    App.HasSessionDataVaultKey env
  ) =>
  Wai.Request ->
  m (Maybe Wai.Request)
login req = do
  sessionDataVaultKey <- asks App.getSessionDataVaultKey
  sessionKey <- asks App.getSessionEncryptionKey

  case (lookup "cookie" $ Wai.requestHeaders req)
    >>= lookup "lions_session" . Cookie.parseCookies
    >>= ClientSession.decrypt sessionKey of
    Nothing -> return Nothing
    Just sessionIdDecrypted -> do
      maybeSession <- Session.get (Session.Id $ decodeUtf8 sessionIdDecrypted)
      case maybeSession of
        Nothing -> return Nothing
        Just session@(Session _ _ userId) -> do
          tryParse <- Session.Valid.parse session
          case tryParse of
            -- The error here just says that the session expired
            Left _ -> return Nothing
            Right _ -> do
              maybeRoles <- Role.get userId
              case maybeRoles of
                Nothing -> return Nothing
                Just roles -> do
                  let vault' = Vault.insert sessionDataVaultKey (roles, userId) $ Wai.vault req
                  return . Just $ req {Wai.vault = vault'}
