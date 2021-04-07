{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception.Safe
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import qualified LandingPage.Handlers
import Layout (layout)
import qualified Login.Handlers
import Lucid
import Network.HTTP.Types (status200, status403, status404, status500)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Session.Domain (VaultKey)
import qualified Session.Middleware
import System.Environment (getEnv)
import qualified System.Log.FastLogger as Log
import Text.Read (readEither)
import User.Domain (UserId (..), isAdmin)
import qualified User.Handlers
import qualified Web.ClientSession as ClientSession
import qualified WelcomeMessage.Handlers
import Prelude hiding (id)

errorPage :: Html ()
errorPage =
  layout "Fehler" Nothing $
    div_ [class_ "container p-3 d-flex justify-content-center"] $
      div_ [class_ "row col-6"] $ do
        p_ [class_ "alert alert-secondary", role_ "alert"] "Es ist leider ein Fehler aufgetreten"

noAuthPage :: Html ()
noAuthPage =
  layout "Fehler" Nothing $
    div_ [class_ "container p-3 d-flex justify-content-center"] $
      div_ [class_ "row col-6"] $ do
        p_ [class_ "alert alert-secondary", role_ "alert"] "Du hast keinen Zugriff auf diese Seite"

app ::
  SQLite.Connection ->
  Log.FastLogger ->
  ClientSession.Key ->
  VaultKey ->
  Wai.Application
app conn logger sessionKey vaultKey req send = do
  handleAny send500 $ case Wai.pathInfo req of
    [] ->
      case Wai.requestMethod req of
        "GET" -> authorizedOnly $ \(roles, _) -> LandingPage.Handlers.showLandingPage logger conn roles req >>= html
        _ -> notFound
    -- TODO: translate
    ["edit"] ->
      case Wai.requestMethod req of
        "POST" -> adminOnly $ WelcomeMessage.Handlers.saveNewMessage conn logger req >>= html
        "GET" -> adminOnly $ WelcomeMessage.Handlers.showMessageEditForm conn logger >>= html
        _ -> notFound
    ["nutzer"] ->
      case Wai.requestMethod req of
        "GET" -> authorizedOnly $ \(roles, _) -> LandingPage.Handlers.showLandingPage logger conn roles req >>= html
        _ -> notFound
    ["nutzer", "neu"] ->
      case Wai.requestMethod req of
        "POST" -> adminOnly $ User.Handlers.saveNewUser logger conn req >>= html
        "GET" -> adminOnly $ User.Handlers.showAddUserForm >>= html
        _ -> notFound
    ["nutzer", int, "editieren"] ->
      case readEither (Text.unpack int) of
        Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
        Right (parsed :: Int) ->
          let userId = UserId parsed
           in case Wai.requestMethod req of
                "GET" -> isOwnIdOrAdmin userId $ \(roles, _) -> User.Handlers.showEditUserForm conn roles userId >>= html
                "POST" -> isOwnIdOrAdmin userId $ \(roles, _) -> User.Handlers.updateExistingUser roles logger conn userId req >>= html
                _ -> notFound
    ["nutzer", int] ->
      case Wai.requestMethod req of
        "GET" ->
          case readEither (Text.unpack int) of
            Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
            Right (parsed :: Int) ->
              authorizedOnly $ \(roles, userid) -> User.Handlers.showProfile conn roles parsed userid req >>= html
        _ -> notFound
    ["nutzer", int, "löschen"] ->
      case readEither (Text.unpack int) of
        Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
        Right (parsed :: Int) ->
          let userId = UserId parsed
           in case Wai.requestMethod req of
                "GET" -> adminOnly $ User.Handlers.showDeleteConfirmation conn userId >>= html
                "POST" -> adminOnly $ User.Handlers.deleteUser conn userId >>= html
                _ -> notFound
    ["login"] ->
      case Wai.requestMethod req of
        "POST" -> Login.Handlers.login sessionKey conn logger req send
        "GET" -> Login.Handlers.showLoginForm vaultKey req >>= html
        _ -> notFound
    _ -> notFound
  where
    html = send . Wai.responseLBS status200 [("Content-Type", "text/html; charset=UTF-8")] . renderBS
    send500 e = do
      logger . Log.toLogStr $ show e <> "\n"
      send
        . Wai.responseLBS status500 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        $ errorPage
    notFound =
      send
        . Wai.responseLBS status404 [("Content-Type", "text/plain; charset=UTF-8")]
        . renderBS
        . layout "Nicht gefunden" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Nicht gefunden"
    isOwnIdOrAdmin userToEdit next = do
      authorizedOnly $ \xs@(roles, userid) -> do
        if userToEdit == userid || any isAdmin roles
          then next xs
          else send . Wai.responseLBS status403 [("Content-Type", "text/html; charset=UTF-8")] $ renderBS noAuthPage
    adminOnly = authorized (any isAdmin . fst) . const
    authorizedOnly = authorized (const True)
    authorized check next =
      case Vault.lookup vaultKey $ Wai.vault req of
        Nothing -> throwString "no data in vault but is protected route"
        Just roles ->
          if check roles
            then next roles
            else html noAuthPage

main :: IO ()
main =
  Log.withFastLogger
    (Log.LogStdout Log.defaultBufSize)
    ( \logger -> do
        sqlitePath <- getEnv "LIONS_SQLITE_PATH"
        sessionKey <- ClientSession.getKeyEnv "LIONS_SESSION_KEY"
        vaultKey <- Vault.newKey
        SQLite.withConnection
          sqlitePath
          ( \conn -> do
              SQLite.execute_ conn "PRAGMA foreign_keys"
              logger $ "Running server at port: " <> Log.toLogStr (3000 :: Int) <> "\n"
              run 3000
                . logStdout
                . staticPolicy (addBase "public")
                . Session.Middleware.middleware conn sessionKey vaultKey logger
                $ app conn logger sessionKey vaultKey
          )
    )
