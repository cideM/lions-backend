{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Login.Handlers (login, showLoginForm) where

import App (Environment (..))
import Capability.Reader (HasReader (..), ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BCrypt
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
import Network.HTTP.Types (status302, status401, status500)
import qualified Network.Wai as Wai
import Network.Wai.Session (genSessionId)
import Session.DB (saveSession)
import Session.Domain (Session (..), SessionDataVaultKey, SessionId (..), makeValidSession)
import User.DB (getIdAndPwByEmail)
import Wai (parseParams)
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

genNewSessionExpires :: IO Time.UTCTime
genNewSessionExpires = do
  now <- Time.getCurrentTime
  let thirtyDays = replicate 30 Time.nominalDay
  return $ foldl' (flip Time.addUTCTime) now thirtyDays

login ::
  ( MonadIO m,
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
    getIdAndPwByEmail conn email >>= \case
      Nothing -> onUnauthenticated email formPw "no user found"
      Just (userId, dbPw) -> do
        katipAddContext (sl "user_id" $ show userId) $ do
          case BCrypt.validatePassword (encodeUtf8 dbPw) (encodeUtf8 formPw) of
            False -> onUnauthenticated email formPw "incorrect password"
            True -> do
              expires <- liftIO genNewSessionExpires
              sessionIdRaw <- liftIO $ decodeUtf8 <$> genSessionId
              liftIO (makeValidSession $ Session (SessionId sessionIdRaw) expires userId) >>= \case
                Left e -> onExcept e
                Right s -> do
                  liftIO $ saveSession conn s
                  encryptedSessionId <- liftIO $ ClientSession.encryptIO sessionKey (encodeUtf8 sessionIdRaw)
                  let cookie =
                        LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
                          Cookie.defaultSetCookie
                            { Cookie.setCookieName = "lions_session",
                              Cookie.setCookieValue = encryptedSessionId,
                              Cookie.setCookieExpires = Just expires,
                              Cookie.setCookiePath = Just "/",
                              Cookie.setCookieSecure = env == Production,
                              Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
                              Cookie.setCookieHttpOnly = True
                            }
                  logLocM InfoS "successful login"
                  send
                    . Wai.responseLBS
                      status302
                      [ ("Content-Type", "text/html; charset=UTF-8"),
                        ("Set-Cookie", cookie),
                        ("Location", "/")
                      ]
                    $ renderBS ""
  where
    onExcept e = do
      logLocM ErrorS (showLS e)
      send $ Wai.responseBuilder status500 [] "Interner Fehler"
    onUnauthenticated email pw (e :: Text) = do
      logLocM ErrorS $ showLS e
      send
        . Wai.responseLBS status401 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . render
        $ NotLoggedInValidated
          email
          (Just "Ungültige Kombination aus Email und Passwort")
          pw
          (Just "Ungültige Kombination aus Email und Passwort")

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
