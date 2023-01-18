module Session.Middleware where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Function ((&))
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

-- Try to log the user in based on the session ID found in a specific cookie.
-- If not, redirect to /login
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
  mbReqWithVault <- login
  case mbReqWithVault of
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
  where
    login = do
      vaultKey <- asks App.getSessionDataVaultKey
      encKey <- asks App.getSessionEncryptionKey

      let mbSessionId = getSessionId encKey req
      case mbSessionId of
        Nothing -> return Nothing
        Just sessionId -> do
          mbSession <- Session.get sessionId
          case mbSession of
            Nothing -> return Nothing
            Just session -> do
              mbValid <- Session.Valid.parse session
              case mbValid of
                Left _ -> return Nothing
                Right valid -> do
                  let Session _ _ userId = Session.Valid.unvalid valid
                  mbRoles <- Role.get userId
                  case mbRoles of
                    Nothing -> return Nothing
                    Just roles -> do
                      let vault' = Vault.insert vaultKey (roles, userId) $ Wai.vault req
                      return . Just $ req {Wai.vault = vault'}

    getSessionId encKey request = do
      cookies <- Wai.requestHeaders request & lookup "cookie"
      sessionId <- lookup "lions_session" $ Cookie.parseCookies cookies
      sessionIdDecrypted <- ClientSession.decrypt encKey sessionId
      return . Session.Id $ decodeUtf8 sessionIdDecrypted
