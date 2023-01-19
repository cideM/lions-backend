{-# LANGUAGE TemplateHaskell #-}

module Session.Middleware where

import qualified App
import Control.Error
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vault.Lazy as Vault
import Katip
import qualified Network.HTTP.Types as Types
import qualified Network.Wai as Wai
import Session.Session (Session (..))
import qualified Session.Session as Session
import qualified Session.Valid as Valid
import qualified User.Role.DB as Role
import qualified Wai
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie

data LoginError
  = NoCookies
  | NoSessionCookie
  | CookieDecryptionError
  | NoSession
  | InvalidSession
  | NoRoles
  deriving (Show, Eq, Ord)

-- Try to log the user in based on the session ID found in a specific cookie.
-- If not, redirect to /login
middleware ::
  ( MonadIO m,
    KatipContext m,
    App.HasSessionDataVaultKey env,
    App.HasDb env,
    MonadThrow m,
    App.HasSessionEncryptionKey env,
    MonadReader env m
  ) =>
  Wai.MiddlewareT m
middleware nextApp req send = do
  vaultKey <- asks App.getSessionDataVaultKey
  encKey <- asks App.getSessionEncryptionKey

  runExceptT (login encKey vaultKey req) >>= \case
    Left e -> do
      $(logTM) DebugS $ showLS e

      -- The user doesn't have a valid session. To prevent infinite redirects,
      -- we need to match on the route.
      case Wai.pathInfo req of
        ["login"] -> do
          nextApp req send
        ["passwort", "link"] -> do
          nextApp req send
        ["passwort", "aendern"] -> do
          nextApp req send
        _ -> send $ Wai.responseBuilder Types.status302 [("Location", "/login")] ""
    Right req' -> do
      $(logTM) DebugS "successful login from cookie"
      nextApp req' send
  where
    login encKey vaultKey request = do
      cookieStr <- (lookup "cookie" $ Wai.requestHeaders request) ?? NoCookies
      let cookies = Cookie.parseCookies cookieStr

      encrypted <- lookup "lions_session" cookies ?? NoSessionCookie
      decrypted <- ClientSession.decrypt encKey encrypted ?? CookieDecryptionError
      let sessionID = Session.Id $ Encoding.decodeUtf8 decrypted

      session <- Session.get sessionID ?* NoSession
      validSession <- fmapLT (const InvalidSession) $ Valid.parse session
      let Session _ _ userId = Valid.unvalid validSession

      roles <- Role.get userId ?* NoRoles
      let vault' = Vault.insert vaultKey (roles, userId) $ Wai.vault request
      return $ req {Wai.vault = vault'}

(?*) :: (MonadError e m) => MaybeT m b -> e -> m b
(?*) x e = runMaybeT x >>= liftEither . note e
