{-# LANGUAGE TemplateHaskell #-}

module Login.Login (postLogout, postLogin, getLogin) where

import qualified App
import Control.Error hiding (tryIO, tryJust)
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader.Class (MonadReader, asks)
import Crypto.KDF.BCrypt (validatePassword)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import Form (FormFieldState (..))
import Katip
import Layout (ActiveNavLink (..), layout)
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
      return . LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
        Cookie.defaultSetCookie
          { -- TODO: Extract into environment
            Cookie.setCookieName = "lions_session",
            Cookie.setCookieValue = "",
            Cookie.setCookieExpires = Nothing,
            Cookie.setCookiePath = Just "/",
            Cookie.setCookieSecure = True,
            Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
            Cookie.setCookieHttpOnly = True
          }

(?*) :: (MonadError e m) => MaybeT m b -> e -> m b
(?*) x e = runMaybeT x >>= liftEither . note e

-- Checks if credentials are valid and then generates a new session, stores it
-- in the DB and returns it to the caller. This function tries two different
-- hashing algorithms. One for old password hashes exported from Firebase and
-- the other is a standard BCrypt algorithm for any new passwords.
login ::
  ( MonadIO m,
    MonadThrow m,
    MonadError Text m,
    KatipContext m,
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
  $(logTM) DebugS "running login function"

  -- plaintext password from HTML form
  let pwBS = encodeUtf8 formPw

  (userId, mbSalt, pw) <- User.getCredentials email ?* "no user found"

  -- encrypted password from database
  let pwEncBS = encodeUtf8 pw

  case mbSalt of
    Just salt -> do
      $(logTM) DebugS "found firebase credentials"

      signerKey <- asks App.getScryptSignerKey
      saltSep <- asks App.getScryptSaltSeparator

      let tryVerify =
            verifyPassword
              signerKey
              saltSep
              (encodeUtf8 salt)
              pwEncBS
              pwBS

      case tryVerify of
        Left e -> throwString $ Text.unpack ("error trying to verify firebase pw: " <> e)
        Right ok -> unless ok $ throwError "incorrect password"
    Nothing -> do
      $(logTM) DebugS "found bcrypt credentials"

      unless (validatePassword pwBS pwEncBS) $ do
        $(logTM) DebugS "incorrect password"
        throwError "incorrect password"

      $(logTM) DebugS "successful bcrypt login"

  newSession <- Session.Valid.create userId
  $(logTM) DebugS "created new session"

  let (Session (Session.Id sessionId) expires _) = Session.Valid.unvalid newSession
  Session.Valid.save newSession
  $(logTM) DebugS "saved new session"

  sessionKey <- asks App.getSessionEncryptionKey
  encryptedSessionId <- liftIO $ ClientSession.encryptIO sessionKey (encodeUtf8 sessionId)
  $(logTM) DebugS "encrypted session ID"

  return (encryptedSessionId, expires)

-- POST handler that creates a new session in the DB and sets a cookie with the
-- encrypted session ID
postLogin ::
  ( MonadIO m,
    MonadThrow m,
    MonadReader env m,
    KatipContext m,
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

  (runExceptT $ login email formPw) >>= \case
    Left e -> do
      $(logTM) ErrorS (ls e)
      renderFormInvalid email formPw
    Right (sessionId, expires) -> do
      $(logTM) DebugS "make cookie"
      cookie <- makeCookie sessionId expires
      $(logTM) DebugS "send cookie response"
      send $ Wai.responseLBS status302 [("Set-Cookie", cookie), ("Location", "/")] mempty
  where
    makeCookie sessionId expires = do
      return . LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
        Cookie.defaultSetCookie
          { Cookie.setCookieName = "lions_session",
            Cookie.setCookieValue = sessionId,
            Cookie.setCookieExpires = Just expires,
            Cookie.setCookiePath = Just "/",
            Cookie.setCookieSecure = True,
            Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
            Cookie.setCookieHttpOnly = True
          }

    renderFormInvalid email formPw =
      send
        . Wai.responseLBS status401 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout User.Session.notAuthenticated (Just Login)
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
  return . layout auth (Just Login) $ LoginForm.render auth (LoginForm.FormInput "" "") LoginForm.emptyForm
