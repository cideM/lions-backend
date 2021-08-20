module Login.Login (postLogout, postLogin, getLogin) where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Crypto.BCrypt as BCrypt
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Error as E
import Form (FormFieldState (..))
import qualified Katip as K
import Layout (layout)
import qualified Login.LoginForm as LoginForm
import Lucid
import Network.HTTP.Types (status302, status401)
import qualified Network.Wai as Wai
import Scrypt (verifyPassword)
import Session.Session (Session (..))
import qualified Session.Session as Session
import qualified Session.Valid
import qualified User.Id as User
import User.Role.Role (Role (..))
import qualified User.Session
import qualified User.User as User
import Wai (parseParams)
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

postLogout ::
  ( MonadIO m,
    MonadThrow m,
    App.HasDb env,
    App.HasEnvironment env,
    MonadReader env m
  ) =>
  (Vault.Vault -> Maybe ([Role], User.Id)) ->
  Wai.Request ->
  (Wai.Response -> m a) ->
  m a
postLogout vaultLookup req send = do
  case vaultLookup $ Wai.vault req of
    Nothing -> throwString "logout request but no session in vault"
    Just (_, userId) -> do
      Session.deleteUser userId
      cookie <- logoutCookie
      send
        . Wai.responseLBS
          status302
          [ ("Content-Type", "text/html; charset=UTF-8"),
            ("Set-Cookie", cookie),
            ("Location", "/login")
          ]
        $ renderBS ""
  where
    logoutCookie = do
      env <- asks App.getEnv
      return . LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
        Cookie.defaultSetCookie
          { Cookie.setCookieName = "lions_session",
            Cookie.setCookieValue = "",
            Cookie.setCookieExpires = Nothing,
            Cookie.setCookiePath = Just "/",
            Cookie.setCookieSecure = env == App.Production,
            Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
            Cookie.setCookieHttpOnly = True
          }

-- Checks if credentials are valid and then generates a new session, stores it
-- in the DB and returns it to the caller. This function tries two different
-- hashing algorithms. One for old password hashes exported from Firebase and
-- the other is a standard BCrypt algorithm for any new passwords.
login ::
  ( MonadIO m,
    MonadThrow m,
    E.MonadError Text m,
    K.KatipContext m,
    App.HasEnvironment env,
    App.HasSessionEncryptionKey env,
    App.HasScryptSignerKey env,
    App.HasScryptSaltSeparator env,
    App.HasDb env,
    MonadReader env m
  ) =>
  Text ->
  Text ->
  m (ByteString, Time.UTCTime)
login email formPw = do
  K.logLocM K.DebugS "running login function"

  signerKey <- asks App.getScryptSignerKey
  saltSep <- asks App.getScryptSaltSeparator
  sessionKey <- asks App.getSessionEncryptionKey

  let verifyPassword' = verifyPassword signerKey saltSep
      clientEncrypt = ClientSession.encryptIO sessionKey

  (userId, userSalt, dbPw) <- User.getCredentials email >>= E.note' "no user found"

  let formPw' = encodeUtf8 formPw
  let dbPw' = encodeUtf8 dbPw

  _ <- case encodeUtf8 <$> userSalt of
    -- Firebase credentials
    Just salt -> do
      K.logLocM K.DebugS "found firebase credentials"
      case verifyPassword' salt dbPw' formPw' of
        Left e -> throwString [i|error trying to verify firebase pw: #{e}|]
        Right ok -> E.unless ok $ E.throwError "incorrect password"
    -- BCrypt, new credentials
    Nothing -> do
      K.logLocM K.DebugS "found bcrypt credentials"
      E.unless (BCrypt.validatePassword dbPw' formPw') $ do
        K.logLocM K.DebugS "incorrect password"
        E.throwError "incorrect password"
      K.logLocM K.DebugS "successful bcrypt login"

  newSession <- Session.Valid.create userId
  K.logLocM K.DebugS "created new session"
  let (Session (Session.Id sessionId) expires _) = Session.Valid.unvalid newSession
  Session.Valid.save newSession
  K.logLocM K.DebugS "saved new session"
  encryptedSessionId <- liftIO $ clientEncrypt (encodeUtf8 sessionId)
  K.logLocM K.DebugS "encrypted session ID"
  return (encryptedSessionId, expires)

-- POST handler that creates a new session in the DB and sets a cookie with the
-- encrypted session ID
postLogin ::
  ( MonadIO m,
    MonadThrow m,
    MonadReader env m,
    K.KatipContext m,
    App.HasEnvironment env,
    App.HasSessionEncryptionKey env,
    App.HasScryptSignerKey env,
    App.HasScryptSaltSeparator env,
    App.HasDb env
  ) =>
  Wai.Request ->
  (Wai.Response -> m a) ->
  m a
postLogin req send = do
  params <- liftIO $ parseParams req

  let email = Map.findWithDefault "" "email" params
      formPw = Map.findWithDefault "" "password" params

  (E.runExceptT $ login email formPw) >>= \case
    Left e -> do
      K.logLocM K.ErrorS (K.ls e)
      renderFormInvalid email formPw
    Right (sessionId, expires) -> do
      K.logLocM K.DebugS "make cookie"
      cookie <- makeCookie sessionId expires
      K.logLocM K.DebugS "send cookie response"
      send $ Wai.responseLBS status302 [("Set-Cookie", cookie), ("Location", "/")] mempty
  where
    makeCookie sessionId expires = do
      env <- asks App.getEnv
      return . LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
        Cookie.defaultSetCookie
          { Cookie.setCookieName = "lions_session",
            Cookie.setCookieValue = sessionId,
            Cookie.setCookieExpires = Just expires,
            Cookie.setCookiePath = Just "/",
            Cookie.setCookieSecure = env == App.Production,
            Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
            Cookie.setCookieHttpOnly = True
          }

    renderFormInvalid email formPw =
      send
        . Wai.responseLBS status401 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout User.Session.notAuthenticated
        $ ( LoginForm.render
              User.Session.notAuthenticated
              (LoginForm.FormInput email formPw)
              ( LoginForm.FormState
                  { loginStateEmail = Invalid "Ungültige Kombination aus Email und Passwort",
                    loginStatePassword = Invalid "Ungültige Kombination aus Email und Passwort"
                  }
              )
          )

getLogin :: (MonadIO m) => User.Session.Authentication -> m (Html ())
getLogin auth =
  return . layout auth $ LoginForm.render auth (LoginForm.FormInput "" "") LoginForm.emptyForm
