module Login.Login (logout, login, showLoginForm) where

import Control.Exception.Safe
import qualified Crypto.BCrypt as BCrypt
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as LBS
import qualified Logging.Logging as Logging
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Env (Environment (..))
import qualified Login.LoginForm as LoginForm
import Lucid
import Network.HTTP.Types (status302, status401)
import qualified Network.Wai as Wai
import Scrypt (verifyPassword)
import Session.Session
  ( Session (..),
    SessionDataVaultKey,
    SessionId (..),
    ValidSession (..),
    createNewSession,
    deleteSession,
    getSessionsFromDbByUser,
    saveSession,
  )
import User.DB (getCredentials)
import Wai (parseParams)
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

-- Validates the given password based on the user account that is associated
-- with that email. Tries to validate the password with BCrypt and Firebase
-- Scrypt so that old password hashes from Firebase auth still work.
createSessionId ::
  SQLite.Connection ->
  ByteString -> -- Project's base64_signer_key
  ByteString -> -- Project's base64_salt_separator
  ClientSession.Key ->
  Text ->
  Text ->
  IO (Either Text (ByteString, Time.UTCTime))
createSessionId
  conn
  signerKey
  saltSep
  sessionKey
  email
  formPw = do
    getCredentials conn email >>= \case
      Nothing -> return $ Left "no user found"
      Just (userId, userSalt, dbPw) -> do
        let formPw' = encodeUtf8 formPw
        let dbPw' = encodeUtf8 dbPw
        case userSalt of
          -- Firebase credentials
          Just us -> do
            case verifyPassword (encodeUtf8 us) signerKey saltSep dbPw' formPw' of
              Left e -> throwString [i|error trying to verify firebase pw: #{e}|]
              Right ok ->
                if ok
                  then onSuccess userId
                  else (return $ Left "incorrect password")
          -- BCrypt, new credentials
          Nothing -> do
            if not $ BCrypt.validatePassword dbPw' formPw'
              then return $ Left "incorrect password"
              else onSuccess userId
    where
      onSuccess userId = do
        newSession@(ValidSession (Session (SessionId sessionId) expires _)) <- createNewSession userId
        saveSession conn newSession
        encryptedSessionId <- ClientSession.encryptIO sessionKey (encodeUtf8 sessionId)
        return $ Right (encryptedSessionId, expires)

logout ::
  SQLite.Connection ->
  Environment ->
  SessionDataVaultKey ->
  Wai.Request ->
  (Wai.Response -> IO a) ->
  IO a
logout conn env sessionDataVaultKey req send = do
  case Vault.lookup sessionDataVaultKey $ Wai.vault req of
    Nothing -> throwString "logout request but no session in vault"
    Just (_, userId) -> do
      -- TODO: deleteAllUserSessions
      getSessionsFromDbByUser conn userId >>= \case
        [] -> throwString $ "logout request but no session for user with ID: " <> show userId
        sessions -> do
          mapM_ (deleteSession conn) sessions
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
  SQLite.Connection ->
  Logging.TimedFastLogger -> 
  ByteString -> -- Project's base64_signer_key
  ByteString -> -- Project's base64_salt_separator
  ClientSession.Key ->
  Environment ->
  Wai.Request ->
  (Wai.Response -> IO a) ->
  IO a
login conn logger signerKey saltSep sessionKey env req send = do
  params <- parseParams req
  let email = Map.findWithDefault "" "email" params
      formPw = Map.findWithDefault "" "password" params
  (createSessionId conn signerKey saltSep sessionKey email formPw) >>= \case
    Left _ -> renderFormInvalid email formPw
    Right (sessionId, expires) -> do
      Logging.log logger ("successful login" :: Text)
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
        . LoginForm.form
        $ LoginForm.NotLoggedInValidated
          email
          (Just "Ungültige Kombination aus Email und Passwort")
          formPw
          (Just "Ungültige Kombination aus Email und Passwort")

-- GET handler for showing the login form
showLoginForm :: SessionDataVaultKey -> Wai.Request -> IO (Html ())
showLoginForm sessionDataVaultKey req = do
  let isLoggedIn = isJust . Vault.lookup sessionDataVaultKey $ Wai.vault req
  return . LoginForm.form $ if isLoggedIn then LoginForm.LoggedIn else LoginForm.NotLoggedInNotValidated
