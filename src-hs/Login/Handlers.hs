{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Login.Handlers (logout, login, showLoginForm) where

import App (Environment (..))
import Capability.Reader (HasReader (..), ask)
import Control.Error (note)
import Control.Exception.Safe
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BCrypt
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Katip hiding (Environment)
import Login.Form (LoginFormState (..), render)
import Lucid
import Network.HTTP.Types (status302, status401)
import qualified Network.Wai as Wai
import Network.Wai.Session (genSessionId)
import Session.DB (deleteSession, getSessionFromDbByUser, saveSession)
import Session.Domain (Session (..), SessionDataVaultKey, SessionId (..), makeValidSession)
import User.DB (getIdAndPwByEmail)
import Wai (parseParams)
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

genNewSessionExpires :: (MonadIO m) => m Time.UTCTime
genNewSessionExpires = do
  now <- liftIO Time.getCurrentTime
  let thirtyDays = replicate 30 Time.nominalDay
  return $ foldl' (flip Time.addUTCTime) now thirtyDays

tryLogin ::
  (MonadError Text m, KatipContext m) =>
  SQLite.Connection ->
  ClientSession.Key ->
  Environment ->
  Text ->
  Text ->
  m ByteString
tryLogin conn sessionKey env email formPw = do
  (userId, dbPw) <- getIdAndPwByEmail conn email >>= liftEither . note "no user found"
  katipAddContext (sl "user_id" $ show userId) $ do
    unless (BCrypt.validatePassword (encodeUtf8 dbPw) (encodeUtf8 formPw)) $ throwError "incorrect password"
    expires <- genNewSessionExpires
    sessionIdRaw <- liftIO $ decodeUtf8 <$> genSessionId
    s <- makeValidSession $ Session (SessionId sessionIdRaw) expires userId
    saveSession conn s
    encryptedSessionId <- liftIO $ ClientSession.encryptIO sessionKey (encodeUtf8 sessionIdRaw)
    logLocM InfoS "successful login"
    return . LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
      Cookie.defaultSetCookie
        { Cookie.setCookieName = "lions_session",
          Cookie.setCookieValue = encryptedSessionId,
          Cookie.setCookieExpires = Just expires,
          Cookie.setCookiePath = Just "/",
          Cookie.setCookieSecure = env == Production,
          Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
          Cookie.setCookieHttpOnly = True
        }

logout ::
  ( MonadCatch m,
    KatipContext m,
    HasReader "dbConn" SQLite.Connection m,
    HasReader "appEnv" Environment m,
    HasReader "sessionDataVaultKey" SessionDataVaultKey m
  ) =>
  Wai.Request ->
  (Wai.Response -> m a) ->
  m a
logout req send = do
  conn <- ask @"dbConn"
  env <- ask @"appEnv"
  sessionDataVaultKey <- ask @"sessionDataVaultKey"
  case Vault.lookup sessionDataVaultKey $ Wai.vault req of
    Nothing -> throwString "logout request but no user in vault"
    Just (_, userId) -> do
      katipAddContext (sl "user_id" $ show userId) $ do
        logLocM DebugS "trying to delete session"
        getSessionFromDbByUser conn userId >>= \case
          Nothing -> throwString "logout request but no session for user"
          Just session@(Session sessionId _ _) -> do
            katipAddContext (sl "session_id" $ show sessionId) $ do
              deleteSession conn session
              logLocM DebugS "deleted session"
              send
                . Wai.responseLBS
                  status302
                  [ ("Content-Type", "text/html; charset=UTF-8"),
                    ("Set-Cookie", logoutCookie env),
                    ("Location", "/login")
                  ]
                $ renderBS ""
  where
    logoutCookie env =
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

login ::
  ( MonadCatch m,
    KatipContext m,
    HasReader "dbConn" SQLite.Connection m,
    HasReader "appEnv" Environment m,
    HasReader "sessionKey" ClientSession.Key m
  ) =>
  Wai.Request ->
  (Wai.Response -> m a) ->
  m a
login req send = do
  conn <- ask @"dbConn"
  sessionKey <- ask @"sessionKey"
  params <- liftIO $ parseParams req
  env <- ask @"appEnv"
  let email = Map.findWithDefault "" "email" params
      formPw = Map.findWithDefault "" "password" params
  katipAddContext (sl "email" email) $ do
    runExceptT (tryLogin conn sessionKey env email formPw) >>= \case
      Left err -> do
        logLocM ErrorS $ showLS err
        send
          . Wai.responseLBS status401 [("Content-Type", "text/html; charset=UTF-8")]
          . renderBS
          . render
          $ NotLoggedInValidated
            email
            (Just "Ungültige Kombination aus Email und Passwort")
            formPw
            (Just "Ungültige Kombination aus Email und Passwort")
      Right cookie -> do
        send
          . Wai.responseLBS
            status302
            [ ("Content-Type", "text/html; charset=UTF-8"),
              ("Set-Cookie", cookie),
              ("Location", "/")
            ]
          $ renderBS ""

showLoginForm ::
  ( MonadIO m,
    HasReader "sessionDataVaultKey" SessionDataVaultKey m,
    KatipContext m
  ) =>
  Wai.Request ->
  m (Html ())
showLoginForm req = do
  sessionDataVaultKey <- ask @"sessionDataVaultKey"
  let isLoggedIn = isJust . Vault.lookup sessionDataVaultKey $ Wai.vault req
  return . render $ if isLoggedIn then LoggedIn else NotLoggedInNotValidated
