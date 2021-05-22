{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Session.Middleware (middleware) where

import Capability.Reader (HasReader (..), ask)
import Control.Error (note)
import Control.Exception.Safe
import Control.Monad.Except (MonadError, liftEither, runExceptT)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Katip
import Network.HTTP.Types (status302)
import qualified Network.Wai as Wai
import RequestID.Middleware (RequestIdVaultKey)
import Session.DB (getSessionFromDb)
import Session.Domain (Session (..), SessionDataVaultKey, SessionId (..), makeValidSession)
import User.DB
  ( getRolesFromDb,
  )
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

getSessionIdFromReq :: (MonadError Text m) => ClientSession.Key -> Wai.Request -> m SessionId
getSessionIdFromReq sessionKey req =
  liftEither $
    (note "no cookie header" . lookup "cookie" $ Wai.requestHeaders req)
      >>= note "no session cookie" . lookup "lions_session" . Cookie.parseCookies
      >>= fmap (SessionId . decodeUtf8) . note "empty session cookie" . ClientSession.decrypt sessionKey

tryLogin ::
  (MonadError Text m, KatipContext m, MonadThrow m) =>
  SQLite.Connection ->
  ClientSession.Key ->
  SessionDataVaultKey ->
  Wai.Request ->
  m Wai.Request
tryLogin dbConn sessionKey sessionDataVaultKey req = do
  sessionId <- getSessionIdFromReq sessionKey req
  katipAddContext (sl "session_id" (show sessionId)) $ do
    session@(Session _ _ userId) <- getSessionFromDb dbConn sessionId >>= liftEither . note "no session found"
    katipAddContext (sl "user_id" (show userId)) $ do
      _ <- makeValidSession session
      roles <- getRolesFromDb dbConn userId >>= liftEither . note "no roles found"
      let vault = Wai.vault req
          vault' = Vault.insert sessionDataVaultKey (roles, userId) vault
          req' = req {Wai.vault = vault'}
      logLocM DebugS "successful session authentication in middleware"
      pure req'

type WaiApp m = Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived

middleware ::
  ( MonadCatch m,
    HasReader "sessionDataVaultKey" SessionDataVaultKey m,
    HasReader "requestIdVaultKey" RequestIdVaultKey m,
    HasReader "dbConn" SQLite.Connection m,
    HasReader "sessionKey" ClientSession.Key m,
    KatipContext m
  ) =>
  WaiApp m ->
  WaiApp m
middleware nextApp req send = do
  sessionKey <- ask @"sessionKey"
  dbConn <- ask @"dbConn"
  sessionDataVaultKey <- ask @"sessionDataVaultKey"
  runExceptT (tryLogin dbConn sessionKey sessionDataVaultKey req) >>= \case
    Left e -> do
      logLocM ErrorS (ls e)
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
