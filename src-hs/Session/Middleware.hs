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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Katip
import Network.HTTP.Types (status302, status500)
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

getSessionIdFromReq :: ClientSession.Key -> Wai.Request -> Either Text SessionId
getSessionIdFromReq sessionKey req =
  (note "no cookie header" . lookup "cookie" $ Wai.requestHeaders req)
    >>= note "no session cookie" . lookup "lions_session" . Cookie.parseCookies
    >>= fmap (SessionId . decodeUtf8) . note "empty session cookie" . ClientSession.decrypt sessionKey

type WaiApp m = Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived

-- Consider renewing session ID everytime
middleware ::
  ( MonadIO m,
    MonadCatch m,
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
  handleAny onExcept $
    case getSessionIdFromReq sessionKey req of
      Left e -> onUnauthenticated e
      Right sessionId -> do
        katipAddContext (sl "session_id" (show sessionId)) $ do
          getSessionFromDb dbConn sessionId >>= \case
            Nothing -> onUnauthenticated "no session for id"
            Just session@(Session _ _ userId) -> do
              katipAddContext (sl "user_id" (show userId)) $ do
                (liftIO $ makeValidSession session) >>= \case
                  Left e -> onUnauthenticated e
                  Right _ -> do
                    getRolesFromDb dbConn userId >>= \case
                      Nothing -> onUnauthenticated "no roles for user"
                      Just roles -> do
                        let vault = Wai.vault req
                            vault' = Vault.insert sessionDataVaultKey (roles, userId) vault
                            req' = req {Wai.vault = vault'}
                        logLocM InfoS "successful session authentication in middleware"
                        nextApp req' send
  where
    onExcept e = do
      logLocM ErrorS (showLS e)
      send $ Wai.responseBuilder status500 [] "Interner Fehler"
    onUnauthenticated (e :: Text) = do
      logLocM InfoS (ls e)
      case Wai.pathInfo req of
        ["login"] -> do
          nextApp req send
        _ -> do
          send $ Wai.responseBuilder status302 [("Location", "/login")] ""
