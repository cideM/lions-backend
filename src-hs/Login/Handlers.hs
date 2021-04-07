{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Login.Handlers (login, showLoginForm) where

import Control.Error (note, runExceptT)
import Control.Monad (unless)
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.BCrypt as BCrypt
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Login.Form (LoginFormState (..), render)
import Lucid
import Network.HTTP.Types (status302, status401)
import qualified Network.Wai as Wai
import Network.Wai.Session (genSessionId)
import Session.DB (saveSession)
import Session.Domain (Session (..), SessionId (..), VaultKey, makeValidSession)
import qualified System.Log.FastLogger as Log
import TextShow
import User.DB (getIdAndPwByEmail)
import Wai (parseParams)
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

genNewSessionCookie :: ByteString -> Time.UTCTime -> ByteString
genNewSessionCookie encryptedSessionId expires =
  LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
    Cookie.defaultSetCookie
      { Cookie.setCookieName = "lions_session",
        Cookie.setCookieValue = encryptedSessionId,
        Cookie.setCookieExpires = Just expires,
        Cookie.setCookiePath = Just "/",
        -- TODO: Needs to depend on env dev vs. prod
        Cookie.setCookieSecure = False,
        Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
        Cookie.setCookieHttpOnly = True
      }

genNewSessionExpires :: IO Time.UTCTime
genNewSessionExpires = do
  now <- Time.getCurrentTime
  let thirtyDays = replicate 30 Time.nominalDay
  return $ foldl' (flip Time.addUTCTime) now thirtyDays

login ::
  ClientSession.Key ->
  SQLite.Connection ->
  Log.FastLogger ->
  Wai.Request ->
  (Wai.Response -> IO a) ->
  IO a
login sessionKey conn logger req send = do
  params <- parseParams req
  let email = Map.findWithDefault "" "email" params
      formPw = Map.findWithDefault "" "password" params
  doLogin email formPw >>= \case
    Left e -> do
      logger . Log.toLogStr $ show e <> "\n"
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
      logger "successful login\n"
      send
        . Wai.responseLBS
          status302
          [ ("Content-Type", "text/html; charset=UTF-8"),
            ("Set-Cookie", cookie),
            ("Location", "/")
          ]
        $ renderBS ""
  where
    doLogin email formPw =
      runExceptT
        ( do
            (userId, dbPw) <- getIdAndPwByEmail conn email >>= liftEither . note ("no user found for ID: " <> showt email)
            unless (BCrypt.validatePassword (encodeUtf8 dbPw) (encodeUtf8 formPw)) $ throwError "invalid credentials"
            expires <- liftIO genNewSessionExpires
            sessionIdRaw <- decodeUtf8 <$> liftIO genSessionId
            s <- makeValidSession $ Session (SessionId sessionIdRaw) expires userId
            liftIO $ saveSession conn s
            encryptedSessionId <- liftIO $ ClientSession.encryptIO sessionKey (encodeUtf8 sessionIdRaw)
            return $ genNewSessionCookie encryptedSessionId expires
        )

showLoginForm ::
  VaultKey ->
  Wai.Request ->
  IO (Html ())
showLoginForm vaultKey req = do
  let isLoggedIn = isJust . Vault.lookup vaultKey $ Wai.vault req
  return . render $ if isLoggedIn then LoggedIn else NotLoggedInNotValidated
