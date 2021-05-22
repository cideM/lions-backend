{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import App (App (..), Env (..), Environment, LogEnv (..), parseEnv)
import Capability.Reader (HasReader (..), ask)
import Control.Exception.Safe
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as Text
import Data.UUID (toText)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Events.Domain (EventId (..))
import qualified Events.Handlers
import qualified Katip as K
import qualified LandingPage.Handlers
import Layout (layout)
import qualified Login.Handlers
import Lucid
import Network.HTTP.Types (status200, status403, status404, status500)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified PasswordReset.Handlers
import RequestID.Middleware (RequestIdVaultKey)
import qualified RequestID.Middleware
import qualified Routes.Data as Auth
import Session.Domain (SessionDataVaultKey)
import qualified Session.Middleware
import System.Environment (getEnv)
import System.IO (stdout)
import Text.Read (readEither)
import User.Domain (UserId (..), isAdmin)
import qualified User.Handlers
import qualified Web.ClientSession as ClientSession
import qualified WelcomeMessage.Handlers
import WelcomeMessage.Domain (WelcomeMsgId (..))
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
      requestId = maybe "" toText $ Vault.lookup requestIdVaultKey vault
      adminOnlyOrOwn id next = case routeData of
        Auth.IsAuthenticated auth@(Auth.IsAdmin _) -> next (id, auth)
        Auth.IsAuthenticated auth@(Auth.IsUser Auth.UserSession {Auth.userSessionUserId = userId}) ->
          if userId == id then next (id, auth) else send403
        _ -> send403
      adminOnly' next = case routeData of
        Auth.IsAuthenticated (Auth.IsAdmin auth) -> next auth
        _ -> send403
      authenticatedOnly' next = case routeData of
        Auth.IsAuthenticated auth -> next auth
        _ -> send403
      -- TODO: Add sessionid to session data
      routeData = case Vault.lookup sessionDataVaultKey vault of
        Nothing -> Auth.IsNotAuthenticated
        Just (roles, userid) ->
          Auth.IsAuthenticated $
            if any isAdmin roles
              then Auth.IsAdmin . Auth.AdminUser $ Auth.UserSession userid roles
              else Auth.IsUser $ Auth.UserSession userid roles
  K.katipAddContext (K.sl "request_id" requestId) $ do
    case Wai.pathInfo req of
      [] ->
        case Wai.requestMethod req of
          -- TODO: I think every handler can just access stuff through vault,
          -- no need for authorizedOnly to call the callback with anything.
          -- It's validation rather than parsing which sucks. What would be an
          -- appropriate type?
          -- data RouteData = NotLoggedIn | Authorized
          -- data FooData = FooData { userid, roles, sessionid }
          -- Interesting question is arguments vs. App vs. Vault, what goes where?
          -- No handler should talk to Vault, they all just need to pattern
          -- match on route data and handle all cases, OR they don't because
          -- app already did that and now they just expect Authorized or Admin.
          -- data Authorized = AuthorizedUser FooData | AuthorizedAdmin FooData
          -- I think I like that
          "GET" -> authenticatedOnly' (LandingPage.Handlers.showLandingPage req >=> send200)
          _ -> send404
      ["veranstaltungen"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' (Events.Handlers.showAllEvents >=> send200)
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", "neu"] ->
        case Wai.requestMethod req of
          "GET" -> adminOnly' (Events.Handlers.showCreateEvent >=> send200)
          "POST" -> adminOnly' (Events.Handlers.handleCreateEvent req >=> send200)
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> authenticatedOnly' (Events.Handlers.showEvent (EventId parsed) send)
              _ -> send404
      ["veranstaltungen", i, "antwort"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "POST" -> authenticatedOnly' (Events.Handlers.replyToEvent req send (EventId parsed))
              _ -> send404
      ["veranstaltungen", i, "loeschen"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' (Events.Handlers.showDeleteEventConfirmation (EventId parsed) >=> send200)
              "POST" -> adminOnly' (Events.Handlers.handleDeleteEvent (EventId parsed) >=> send200)
              _ -> send404
      ["veranstaltungen", i, "editieren"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' (Events.Handlers.showEditEventForm (EventId parsed) >=> send200)
              "POST" -> adminOnly' (Events.Handlers.handleUpdateEvent req (EventId parsed) >=> send200)
              _ -> send404
      ["loeschen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = (WelcomeMsgId parsed)
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' (WelcomeMessage.Handlers.handleDeleteMessage msgId >=> send200)
                  "GET" -> adminOnly' (WelcomeMessage.Handlers.showDeleteConfirmation msgId >=> send200)
                  _ -> send404
      ["neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' (WelcomeMessage.Handlers.saveNewMessage req >=> send200)
          "GET" -> adminOnly' (WelcomeMessage.Handlers.showAddMessageForm >=> send200)
          _ -> send404
      ["editieren", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = (WelcomeMsgId parsed)
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' (WelcomeMessage.Handlers.handleEditMessage req msgId >=> send200)
                  "GET" -> adminOnly' (WelcomeMessage.Handlers.showMessageEditForm msgId >=> send200)
                  _ -> send404
      ["nutzer"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' (User.Handlers.showMemberList req >=> send200)
          _ -> send404
      ["nutzer", "neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' (User.Handlers.saveNewUser req >=> send200)
          "GET" -> adminOnly' (User.Handlers.showAddUserForm >=> send200)
          _ -> send404
      ["nutzer", int, "editieren"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
          Right (parsed :: Int) ->
            let userId = UserId parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnlyOrOwn userId (uncurry User.Handlers.showEditUserForm >=> send200)
                  "POST" -> adminOnlyOrOwn userId (uncurry (User.Handlers.updateExistingUser req) >=> send200)
                  _ -> send404
      ["nutzer", int] ->
        case Wai.requestMethod req of
          "GET" ->
            case readEither (Text.unpack int) of
              Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
              Right (parsed :: Int) ->
                authenticatedOnly' (User.Handlers.showProfile parsed >=> send200)
          _ -> send404
      ["nutzer", int, "loeschen"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
          Right (parsed :: Int) ->
            let userId = UserId parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnly' (User.Handlers.showDeleteConfirmation userId >=> send200)
                  "POST" -> adminOnly' (User.Handlers.deleteUser userId >=> send200)
                  _ -> send404
      ["login"] ->
        case Wai.requestMethod req of
          "POST" -> Login.Handlers.login req send
          "GET" -> Login.Handlers.showLoginForm req >>= send200
          _ -> send404
      ["logout"] ->
        case Wai.requestMethod req of
          "POST" -> Login.Handlers.logout req send
          _ -> send404
      ["passwort", "aendern"] ->
        case Wai.requestMethod req of
          "GET" -> PasswordReset.Handlers.showChangePwForm req >>= send200
          "POST" -> PasswordReset.Handlers.handleChangePw req >>= send200
          _ -> send404
      ["passwort", "link"] ->
        case Wai.requestMethod req of
          "GET" -> PasswordReset.Handlers.showResetForm >>= send200
          "POST" -> PasswordReset.Handlers.handleReset req >>= send200
          _ -> send404
      _ -> send404
  where
    send200 =
      send
        . Wai.responseLBS status200 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
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
        . Wai.responseLBS status404 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout "Nicht gefunden" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Nicht gefunden"

main :: IO ()
main = do
  sqlitePath <- getEnv "LIONS_SQLITE_PATH"
  appEnv <- getEnv "LIONS_ENV" >>= parseEnv
  sessionKeyFile <- getEnv "LIONS_SESSION_KEY_FILE"
  sessionKey <- ClientSession.getKey sessionKeyFile
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
              liftIO . (runSettings . setPort 3000 $ setHost "localhost" defaultSettings)
                . logStdout
                . staticPolicy (addBase "public")
                $ ( \r s ->
                      let send = liftIO . s
                       in flip unApp env
                            . handleAny (send500 send)
                            $ (RequestID.Middleware.middleware . Session.Middleware.middleware) app r send
                  )
    )
  where
    send500 send e = do
      K.logLocM K.ErrorS $ K.showLS e
      send
        . Wai.responseLBS status500 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout "Fehler" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Es ist leider ein Fehler aufgetreten"
