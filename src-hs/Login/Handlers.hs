{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Login.Handlers (login, showLoginForm) where

import Control.Error (note, runExceptT)
import Control.Monad (unless)
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.BCrypt as BCrypt
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Login.Form (LoginFormState (..), render)
import Lucid
import Network.HTTP.Types (status401)
import qualified Network.Wai as Wai
import Network.Wai.Session (genSessionId)
import Session.DB (saveSession)
import Session.Domain (Session (..), SessionId (..), makeValidSession, VaultKey)
import qualified System.Log.FastLogger as Log
import TextShow
import User.DB (getIdAndPwByEmail)
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SCookie
import Prelude hiding (id)

genNewSessionCookie :: ByteString -> Time.UTCTime -> Cookie.SetCookie
genNewSessionCookie encryptedSessionId expires =
  Cookie.def
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
  S.ActionM ()
login sessionKey conn logger = do
  email <- S.param "email" `S.rescue` const (return "")
  formPw <- S.param "password" `S.rescue` const (return "")
  S.liftAndCatchIO (doLogin email formPw) >>= \case
    Left e -> do
      S.liftAndCatchIO . logger . Log.toLogStr $ show e <> "\n"
      S.status status401
      S.html . renderText
        . render
        $ NotLoggedInValidated
          email
          (Just "Ungültige Kombination aus Email und Passwort")
          formPw
          (Just "Ungültige Kombination aus Email und Passwort")
    Right cookie -> do
      S.liftAndCatchIO $ logger "successful login\n"
      SCookie.setCookie cookie
      S.redirect "/"
  where
    doLogin email formPw = do
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
  S.ActionM ()
showLoginForm vaultKey = do
  isLoggedIn <- S.request >>= return . isJust . Vault.lookup vaultKey . Wai.vault
  S.html . renderText . render $ if isLoggedIn then LoggedIn else NotLoggedInNotValidated
