{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Login.Handlers (logout, login, showLoginForm) where

import App (Environment (..))
import Control.Exception.Safe
import qualified Crypto.BCrypt as BCrypt
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Login.Form (LoginFormState (..), render)
import Lucid
import Network.HTTP.Types (status302, status401)
import qualified Network.Wai as Wai
import Session.DB (deleteSession, getSessionsFromDbByUser, saveSession)
import Session.Domain (Session (..), SessionDataVaultKey, SessionId (..), ValidSession (..), createNewSession)
import User.DB (getIdAndPwByEmail)
import Wai (parseParams)
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

tryLogin ::
  SQLite.Connection ->
  ClientSession.Key ->
  Environment ->
  Text ->
  Text ->
  IO (Either Text ByteString)
tryLogin conn sessionKey env email formPw = do
  getIdAndPwByEmail conn email >>= \case
    Nothing -> return $ Left "no user found"
    Just (userId, dbPw) -> do
      if not $ BCrypt.validatePassword (encodeUtf8 dbPw) (encodeUtf8 formPw)
        then return $ Left "incorrect password"
        else do
          newSession@(ValidSession (Session (SessionId sessionId) expires _)) <- createNewSession userId
          saveSession conn newSession
          encryptedSessionId <- ClientSession.encryptIO sessionKey (encodeUtf8 sessionId)
          return . Right . LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
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

login ::
  SQLite.Connection ->
  ClientSession.Key ->
  Environment ->
  Wai.Request ->
  (Wai.Response -> IO a) ->
  IO a
login conn sessionKey env req send = do
  params <- parseParams req
  let email = Map.findWithDefault "" "email" params
      formPw = Map.findWithDefault "" "password" params
  (tryLogin conn sessionKey env email formPw) >>= \case
    Left _ -> do
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
      send $
        Wai.responseLBS
          status302
          [ ("Set-Cookie", cookie),
            ("Location", "/")
          ]
          mempty

showLoginForm :: SessionDataVaultKey -> Wai.Request -> IO (Html ())
showLoginForm sessionDataVaultKey req = do
  let isLoggedIn = isJust . Vault.lookup sessionDataVaultKey $ Wai.vault req
  return . render $ if isLoggedIn then LoggedIn else NotLoggedInNotValidated
