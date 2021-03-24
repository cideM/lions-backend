-- TODO: Extract routes and magic strings ioto vars
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import qualified LandingPage.Handlers
import Layout (layout)
import qualified Login.Handlers
import Lucid
import qualified Network.Wai as Wai
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Session.Domain (VaultKey)
import qualified Session.Middleware
import System.Environment (getEnv)
import qualified System.Log.FastLogger as Log
import User.Domain (UserId (..), isAdmin)
import qualified User.Handlers
import qualified Web.ClientSession as ClientSession
import qualified Web.Scotty as S
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
  S.ScottyM ()
app conn logger sessionKey vaultKey = do
  S.get "/" $ authorized (const True) $ \(roles, _) -> LandingPage.Handlers.showLandingPage logger conn roles
  -- TODO: translate
  S.post "/edit" $ adminOnly . const $ WelcomeMessage.Handlers.saveNewMessage conn logger
  S.get "/edit" $ adminOnly . const $ WelcomeMessage.Handlers.showMessageEditForm conn logger
  S.get "/nutzer" $ authorized (const True) $ \(roles, _) -> LandingPage.Handlers.showLandingPage logger conn roles
  S.post "/nutzer/neu" $ adminOnly . const $ User.Handlers.saveNewUser logger conn
  S.get "/nutzer/neu" $ adminOnly . const $ User.Handlers.showAddUserForm
  S.get "/nutzer/:id/editieren" $ do
    authorized (const True) $ \(roles, userid) -> do
      (i :: Int) <- S.param "id"
      let userToEdit = UserId i
      if userToEdit == userid || any isAdmin roles
        then User.Handlers.showEditUserForm conn roles userToEdit
        else S.html $ renderText noAuthPage
  S.post "/nutzer/:id/editieren" $ do
    authorized (const True) $ \(roles, userid) -> do
      (i :: Int) <- S.param "id"
      let userToEdit = UserId i
      if userToEdit == userid || any isAdmin roles
        then User.Handlers.updateExistingUser roles logger conn
        else S.html $ renderText noAuthPage
  S.get "/nutzer/:id" $ authorized (const True) $ \(roles, loggedInUserId) -> User.Handlers.showProfile conn roles loggedInUserId
  S.get "/nutzer/:id/löschen" $ adminOnly . const $ User.Handlers.showDeleteConfirmation conn
  S.post "/nutzer/:id/löschen" $ adminOnly . const $ User.Handlers.deleteUser conn
  S.post "/login" $ Login.Handlers.login sessionKey conn logger
  S.get "/login" $ Login.Handlers.showLoginForm vaultKey
  where
    adminOnly = authorized (any isAdmin . fst)
    authorized check next =
      S.request >>= \req -> case Vault.lookup vaultKey $ Wai.vault req of
        Nothing -> S.raise "no data in vault but is protected route"
        Just roles ->
          if check roles
            then next roles
            else S.html $ renderText noAuthPage

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
              S.scotty 3000 $ do
                -- Must come first
                S.defaultHandler $ \e -> do
                  liftIO $ logger . Log.toLogStr $ e <> "\n"
                  S.html . renderText $ errorPage
                S.middleware logStdout
                S.middleware $ staticPolicy (addBase "public")
                S.middleware $ Session.Middleware.middleware conn sessionKey vaultKey logger
                app conn logger sessionKey vaultKey
                S.notFound $ do
                  S.html . renderText . layout "Nicht gefunden" Nothing $
                    div_ [class_ "container p-3 d-flex justify-content-center"] $
                      div_ [class_ "row col-6"] $ do
                        p_ [class_ "alert alert-secondary", role_ "alert"] "Nicht gefunden"
          )
    )
