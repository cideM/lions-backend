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
import Layout (layout)
import qualified Login.LoginForm as LoginForm
import Lucid
import Network.HTTP.Types (status302, status401)
import qualified Network.Wai as Wai
import Scrypt (verifyPassword)
import qualified Session.DB as Session
import Session.Types
  ( Authentication (..),
    Session (..),
    SessionId (..),
    ValidSession (..),
  )
import qualified Session.Valid as Session
import User.DB (getCredentials)
import User.Types (Role, UserId (..))
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
  (Vault.Vault -> Maybe ([Role], UserId)) ->
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
  conn <- asks App.getDb
  signerKey <- asks App.getScryptSignerKey
  saltSep <- asks App.getScryptSaltSeparator
  sessionKey <- asks App.getSessionEncryptionKey

  let verifyPassword' = verifyPassword signerKey saltSep
      clientEncrypt = ClientSession.encryptIO sessionKey

  (userId, userSalt, dbPw) <- (liftIO $ getCredentials conn email) >>= E.note' "no user found"

  let formPw' = encodeUtf8 formPw
  let dbPw' = encodeUtf8 dbPw

  _ <- case encodeUtf8 <$> userSalt of
    -- Firebase credentials
    Just salt -> do
      case verifyPassword' salt dbPw' formPw' of
        Left e -> throwString [i|error trying to verify firebase pw: #{e}|]
        Right ok -> E.unless ok $ E.throwError "incorrect password"
    -- BCrypt, new credentials
    Nothing ->
      E.unless (BCrypt.validatePassword dbPw' formPw') $ E.throwError "incorrect password"

  newSession@(ValidSession (Session (SessionId sessionId) expires _)) <- Session.create userId
  Session.save newSession
  encryptedSessionId <- liftIO $ clientEncrypt (encodeUtf8 sessionId)
  return (encryptedSessionId, expires)

-- POST handler that creates a new session in the DB and sets a cookie with the
-- encrypted session ID
postLogin ::
  ( MonadIO m,
    App.HasEnvironment env,
    App.HasEnvironment env,
    App.HasSessionEncryptionKey env,
    App.HasScryptSignerKey env,
    MonadThrow m,
    App.HasScryptSaltSeparator env,
    App.HasDb env,
    MonadReader env m
  ) =>
  Wai.Request ->
  (Wai.Response -> m a) ->
  m a
postLogin req send = do
  params <- liftIO $ parseParams req

  let email = Map.findWithDefault "" "email" params
      formPw = Map.findWithDefault "" "password" params

  (E.runExceptT $ login email formPw) >>= \case
    Left _ -> renderFormInvalid email formPw
    Right (sessionId, expires) -> do
      cookie <- makeCookie sessionId expires
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
        . layout IsNotAuthenticated
        . LoginForm.form
        $ LoginForm.NotLoggedInValidated
          email
          (Just "Ungültige Kombination aus Email und Passwort")
          formPw
          (Just "Ungültige Kombination aus Email und Passwort")

getLogin :: (MonadIO m) => Authentication -> m (Html ())
getLogin (IsNotAuthenticated) =
  return . layout IsNotAuthenticated . LoginForm.form $ LoginForm.NotLoggedInNotValidated
getLogin auth = return . layout auth . LoginForm.form $ LoginForm.LoggedIn
