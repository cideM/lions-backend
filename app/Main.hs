{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import App (App (..), Env (..), Environment, LogEnv (..), parseEnv)
import Capability.Reader (HasReader (..), ask)
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.UUID (toText)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import qualified Katip as K
import qualified LandingPage.Handlers
import Layout (layout)
import qualified Login.Handlers
import Lucid
import Network.HTTP.Types (status200, status403, status404, status500)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import RequestID.Middleware (RequestIdVaultKey)
import qualified RequestID.Middleware
import Session.Domain (SessionDataVaultKey)
import qualified Session.Middleware
import System.Environment (getEnv)
import System.IO (stdout)
import Text.Read (readEither)
import User.Domain (UserId (..), isAdmin)
import qualified User.Handlers
import qualified Web.ClientSession as ClientSession
import qualified WelcomeMessage.Handlers
import Prelude hiding (id)

app ::
  ( MonadIO m,
    MonadCatch m,
    HasReader "dbConn" SQLite.Connection m,
    HasReader "sessionKey" ClientSession.Key m,
    HasReader "requestIdVaultKey" RequestIdVaultKey m,
    HasReader "appEnv" Environment m,
    HasReader "logContexts" K.LogContexts m,
    K.KatipContext m,
    HasReader "sessionDataVaultKey" SessionDataVaultKey m
  ) =>
  Wai.Request ->
  (Wai.Response -> m Wai.ResponseReceived) ->
  m Wai.ResponseReceived
app req send = do
  sessionDataVaultKey <- ask @"sessionDataVaultKey"
  requestIdVaultKey <- ask @"requestIdVaultKey"
  let vault = Wai.vault req
      requestId = fromMaybe "" $ toText <$> Vault.lookup requestIdVaultKey vault
      adminOnly = authorized (any isAdmin . fst) . const
      authorizedOnly = authorized (const True)
      authorized check next = do
        case Vault.lookup sessionDataVaultKey vault of
          Nothing -> throwString "no data in vault but is protected route"
          Just roles ->
            if check roles
              then next roles
              else send403
      isOwnIdOrAdmin userToEdit next = do
        authorizedOnly $ \xs@(roles, userid) -> do
          if userToEdit == userid || any isAdmin roles
            then next xs
            else send403
  K.katipAddContext (K.sl "request_id" requestId) $ do
    handleAny send500 $ case Wai.pathInfo req of
      [] ->
        case Wai.requestMethod req of
          "GET" -> authorizedOnly $ \(roles, _) -> LandingPage.Handlers.showLandingPage roles req >>= send200
          _ -> send404
      -- TODO: translate
      ["edit"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly $ WelcomeMessage.Handlers.saveNewMessage req >>= send200
          "GET" -> adminOnly $ WelcomeMessage.Handlers.showMessageEditForm >>= send200
          _ -> send404
      ["nutzer"] ->
        case Wai.requestMethod req of
          "GET" -> authorizedOnly $ \(roles, _) -> LandingPage.Handlers.showLandingPage roles req >>= send200
          _ -> send404
      ["nutzer", "neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly $ User.Handlers.saveNewUser req >>= send200
          "GET" -> adminOnly $ User.Handlers.showAddUserForm >>= send200
          _ -> send404
      ["nutzer", int, "editieren"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
          Right (parsed :: Int) ->
            let userId = UserId parsed
             in case Wai.requestMethod req of
                  "GET" -> isOwnIdOrAdmin userId $ \(roles, _) -> User.Handlers.showEditUserForm roles userId >>= send200
                  "POST" -> isOwnIdOrAdmin userId $ \(roles, _) -> User.Handlers.updateExistingUser roles userId req >>= send200
                  _ -> send404
      ["nutzer", int] ->
        case Wai.requestMethod req of
          "GET" ->
            case readEither (Text.unpack int) of
              Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
              Right (parsed :: Int) ->
                authorizedOnly $ \(roles, userid) -> User.Handlers.showProfile roles parsed userid req >>= send200
          _ -> send404
      ["nutzer", int, "löschen"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
          Right (parsed :: Int) ->
            let userId = UserId parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnly $ User.Handlers.showDeleteConfirmation userId >>= send200
                  "POST" -> adminOnly $ User.Handlers.deleteUser userId >>= send200
                  _ -> send404
      ["login"] ->
        case Wai.requestMethod req of
          "POST" -> Login.Handlers.login req send
          "GET" -> Login.Handlers.showLoginForm req >>= send200
          _ -> send404
      _ -> send404
  where
    send200 =
      send
        . Wai.responseLBS status200 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
    send500 e = do
      K.logLocM K.ErrorS $ K.showLS e
      send
        . Wai.responseLBS status500 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout "Fehler" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Es ist leider ein Fehler aufgetreten"
    send403 =
      send
        . Wai.responseLBS status403 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout "Fehler" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Du hast keinen Zugriff auf diese Seite"
    send404 =
      send
        . Wai.responseLBS status404 [("Content-Type", "text/plain; charset=UTF-8")]
        . renderBS
        . layout "Nicht gefunden" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Nicht gefunden"

main :: IO ()
main = do
  sqlitePath <- getEnv "LIONS_SQLITE_PATH"
  appEnv <- getEnv "LIONS_ENV" >>= parseEnv
  sessionKey <- ClientSession.getKeyEnv "LIONS_SESSION_KEY"
  sessionDataVaultKey <- Vault.newKey
  requestIdVaultKey <- Vault.newKey
  SQLite.withConnection
    sqlitePath
    ( \conn -> do
        handleScribe <-
          K.mkHandleScribe
            K.ColorIfTerminal
            stdout
            (K.permitItem K.InfoS)
            K.V2

        let initialContext = ()
            initialNamespace = "main"
            makeLogEnv =
              K.registerScribe
                "stdout"
                handleScribe
                K.defaultScribeSettings
                =<< K.initLogEnv "Lions Server" (K.Environment . Text.pack $ show appEnv)

        bracket makeLogEnv K.closeScribes $ \katipLogEnv -> do
          K.runKatipContextT katipLogEnv initialContext initialNamespace $ do
            ctx <- K.getKatipContext
            let env = Env conn sessionKey sessionDataVaultKey appEnv requestIdVaultKey $ LogEnv ctx initialNamespace katipLogEnv

            liftIO $ SQLite.execute_ conn "PRAGMA foreign_keys"
            K.katipAddContext (K.sl "port" (3000 :: Int)) $ do
              K.logLocM K.InfoS "starting server"
              liftIO . run 3000
                . logStdout
                . staticPolicy (addBase "public")
                $ ( \r s ->
                      flip unApp env $
                        (RequestID.Middleware.middleware . Session.Middleware.middleware) app r (liftIO . s)
                  )
    )
