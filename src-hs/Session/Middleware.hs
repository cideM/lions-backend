{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Session.Middleware (middleware) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Logging.Logging as Logging
import qualified Database.SQLite.Simple as SQLite
import Network.HTTP.Types (status302)
import qualified Network.Wai as Wai
import Session.DB (getSessionFromDb)
import Session.Domain (Session (..), SessionDataVaultKey, SessionId (..), makeValidSession)
import User.DB
  ( getRolesFromDb,
  )
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

getSessionIdFromReq :: ClientSession.Key -> Wai.Request -> Either Text SessionId
getSessionIdFromReq sessionKey req =
  case lookup "cookie" $ Wai.requestHeaders req of
    Nothing -> Left "no cookie header"
    Just cookie -> case lookup "lions_session" $ Cookie.parseCookies cookie of
      Nothing -> Left "no session cookie"
      Just session ->
        case ClientSession.decrypt sessionKey session of
          Nothing -> Left "empty session cookie"
          Just decrypted -> Right . SessionId $ decodeUtf8 decrypted

tryLogin ::
  SQLite.Connection ->
  ClientSession.Key ->
  SessionDataVaultKey ->
  Wai.Request ->
  IO (Either Text Wai.Request)
tryLogin dbConn sessionKey sessionDataVaultKey req = do
  case getSessionIdFromReq sessionKey req of
    Left err -> return $ Left err
    Right sessionId -> do
      getSessionFromDb dbConn sessionId >>= \case
        Nothing -> return $ Left "no session found"
        Just session@(Session _ _ userId) -> do
          _ <- makeValidSession session
          getRolesFromDb dbConn userId >>= \case
            Nothing -> return $ Left "no roles found"
            Just roles -> do
              let vault = Wai.vault req
                  vault' = Vault.insert sessionDataVaultKey (roles, userId) vault
                  req' = req {Wai.vault = vault'}
              return $ Right req'

middleware ::
  Logging.TimedFastLogger ->
  SessionDataVaultKey ->
  SQLite.Connection ->
  ClientSession.Key ->
  Wai.Application ->
  Wai.Application
middleware _ sessionDataVaultKey dbConn sessionKey nextApp req send = do
  tryLogin dbConn sessionKey sessionDataVaultKey req >>= \case
    Left _ -> do
      case Wai.pathInfo req of
        ["login"] -> do
          nextApp req send
        ["passwort", "link"] -> do
          nextApp req send
        ["passwort", "aendern"] -> do
          nextApp req send
        _ -> do
          send $ Wai.responseBuilder status302 [("Location", "/login")] ""
    Right req' -> nextApp req' send
