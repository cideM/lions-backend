module Login.Login (logout, login, showLoginForm) where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import Env (Environment (..))
import Layout (layout)
import qualified Logging as Logging
import qualified Login.LoginForm as LoginForm
import Lucid
import Network.HTTP.Types (status302, status401)
import qualified Network.Wai as Wai
import Session (Authentication (..))
import User.Types (Role, UserId (..))
import Wai (parseParams)
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

logout ::
  (MonadIO m, MonadThrow m) =>
  (UserId -> IO ()) ->
  (Vault.Vault -> Maybe ([Role], UserId)) ->
  Environment ->
  Wai.Request ->
  (Wai.Response -> m a) ->
  m a
logout deleteSessions vaultLookup env req send =
  case vaultLookup $ Wai.vault req of
    Nothing -> throwString "logout request but no session in vault"
    Just (_, userId) -> do
      liftIO $ deleteSessions userId
      send
        . Wai.responseLBS
          status302
          [ ("Content-Type", "text/html; charset=UTF-8"),
            ("Set-Cookie", logoutCookie env),
            ("Location", "/login")
          ]
        $ renderBS ""
  where
    logoutCookie _ =
      LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
        Cookie.defaultSetCookie
          { Cookie.setCookieName = "lions_session",
            Cookie.setCookieValue = "",
            Cookie.setCookieExpires = Nothing,
            Cookie.setCookiePath = Just "/",
            Cookie.setCookieSecure = env == Production,
            Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
            Cookie.setCookieHttpOnly = True
          }

-- POST handler that creates a new session in the DB and sets a cookie with the
-- encrypted session ID
login ::
  (MonadIO m) =>
  Logging.Log ->
  (Text -> Text -> IO (Either Text (ByteString, Time.UTCTime))) ->
  Environment ->
  Wai.Request ->
  (Wai.Response -> m a) ->
  m a
login _ tryLogin env req send = do
  params <- liftIO $ parseParams req
  let email = Map.findWithDefault "" "email" params
      formPw = Map.findWithDefault "" "password" params
  (liftIO $ tryLogin email formPw) >>= \case
    Left _ -> renderFormInvalid email formPw
    Right (sessionId, expires) -> do
      let cookie = makeCookie sessionId expires
      send $ Wai.responseLBS status302 [("Set-Cookie", cookie), ("Location", "/")] mempty
  where
    makeCookie sessionId expires =
      LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
        Cookie.defaultSetCookie
          { Cookie.setCookieName = "lions_session",
            Cookie.setCookieValue = sessionId,
            Cookie.setCookieExpires = Just expires,
            Cookie.setCookiePath = Just "/",
            Cookie.setCookieSecure = env == Production,
            Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
            Cookie.setCookieHttpOnly = True
          }
    renderFormInvalid email formPw =
      send
        . Wai.responseLBS status401 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout IsNotAuthenticated
        . LoginForm.form
        $ LoginForm.NotLoggedInValidated
          email
          (Just "Ungültige Kombination aus Email und Passwort")
          formPw
          (Just "Ungültige Kombination aus Email und Passwort")

-- GET handler for showing the login form
showLoginForm ::
  (MonadIO m) =>
  Authentication ->
  m (Html ())
showLoginForm (IsNotAuthenticated) =
  return . layout IsNotAuthenticated . LoginForm.form $ LoginForm.NotLoggedInNotValidated
showLoginForm auth = return . layout auth . LoginForm.form $ LoginForm.LoggedIn
