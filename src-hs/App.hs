module App (server, main) where

-- TODO: rename to Main

import Control.Exception.Safe
import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Env (Environment (..), parseEnv)
import Events.Domain (EventId (..))
import qualified Events.Handlers
import qualified LandingPage.Handlers
import Layout (layout)
import qualified Logging.Logging as Logging
import qualified Login.Login as Login
import Lucid
import qualified Network.AWS as AWS
import Network.HTTP.Types (status200, status403, status404, status500)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified PasswordReset.PasswordReset as PasswordReset
import qualified PasswordReset.SendEmail as SendEmail
import RequestID.Middleware (RequestIdVaultKey)
import qualified RequestID.Middleware
import qualified Routes.Data as Auth
import Session.Session (SessionDataVaultKey)
import qualified Session.Session as Session
import System.Environment (getEnv)
import System.Log.FastLogger (LogType' (..), defaultBufSize, newTimeCache, simpleTimeFormat, withTimedFastLogger)
import Text.Read (readEither)
import User.Domain (UserId (..), isAdmin)
import qualified User.Handlers
import qualified Web.ClientSession as ClientSession
import WelcomeMessage.WelcomeMessage (WelcomeMsgId (..))
import qualified WelcomeMessage.WelcomeMessage as WelcomeMessage
import Prelude hiding (id)

server ::
  Logging.TimedFastLogger ->
  SQLite.Connection ->
  ClientSession.Key ->
  RequestIdVaultKey ->
  AWS.Env ->
  Int ->
  Environment ->
  SessionDataVaultKey ->
  BS.ByteString -> -- User's salt
  BS.ByteString -> -- Project's base64_signer_key
  BS.ByteString -> -- Project's base64_salt_separator
  Wai.Request ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  IO Wai.ResponseReceived
server
  logger
  dbConn
  sessionKey
  _
  awsEnv
  port
  appEnv
  sessionDataVaultKey
  userSalt
  signerKey
  saltSep
  req
  send = do
    let vault = Wai.vault req
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
        routeData = case Vault.lookup sessionDataVaultKey vault of
          Nothing -> Auth.IsNotAuthenticated
          Just (roles, userid) ->
            Auth.IsAuthenticated $
              if any isAdmin roles
                then Auth.IsAdmin . Auth.AdminUser $ Auth.UserSession userid roles
                else Auth.IsUser $ Auth.UserSession userid roles
        resetHost =
          if appEnv == Production
            then "https://www.lions-achern.de"
            else Text.pack $ "http://localhost:" <> show port
        sendMail' = SendEmail.sendMail awsEnv resetHost
    case Wai.pathInfo req of
      [] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ \auth -> (LandingPage.Handlers.showLandingPage dbConn auth) >>= send200
          _ -> send404
      ["veranstaltungen"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ \auth -> (Events.Handlers.showAllEvents dbConn auth) >>= send200
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", "neu"] ->
        case Wai.requestMethod req of
          "GET" -> adminOnly' $ \admin -> (Events.Handlers.showCreateEvent admin) >>= send200
          "POST" -> adminOnly' $ \admin -> (Events.Handlers.handleCreateEvent dbConn req admin) >>= send200
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> authenticatedOnly' $ \auth -> Events.Handlers.showEvent dbConn (EventId parsed) send auth
              _ -> send404
      ["veranstaltungen", i, "antwort"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "POST" -> authenticatedOnly' $ \auth -> Events.Handlers.replyToEvent dbConn req send (EventId parsed) auth
              _ -> send404
      ["veranstaltungen", i, "loeschen"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ \admin -> (Events.Handlers.showDeleteEventConfirmation dbConn (EventId parsed) admin) >>= send200
              "POST" -> adminOnly' $ \admin -> (Events.Handlers.handleDeleteEvent dbConn (EventId parsed) admin) >>= send200
              _ -> send404
      ["veranstaltungen", i, "editieren"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ \admin -> (Events.Handlers.showEditEventForm dbConn (EventId parsed) admin) >>= send200
              "POST" -> adminOnly' $ \admin -> (Events.Handlers.handleUpdateEvent dbConn req (EventId parsed) admin) >>= send200
              _ -> send404
      ["loeschen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = (WelcomeMsgId parsed)
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ \admin -> (WelcomeMessage.handleDeleteMessage dbConn msgId admin) >>= send200
                  "GET" -> adminOnly' $ \admin -> (WelcomeMessage.showDeleteConfirmation dbConn msgId admin) >>= send200
                  _ -> send404
      ["neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ \admin -> (WelcomeMessage.saveNewMessage dbConn req admin) >>= send200
          "GET" -> adminOnly' $ \admin -> (WelcomeMessage.showAddMessageForm admin) >>= send200
          _ -> send404
      ["editieren", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = (WelcomeMsgId parsed)
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ \admin -> (WelcomeMessage.handleEditMessage dbConn req msgId admin) >>= send200
                  "GET" -> adminOnly' $ \admin -> (WelcomeMessage.showMessageEditForm dbConn msgId admin) >>= send200
                  _ -> send404
      ["nutzer"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ \auth -> (User.Handlers.showMemberList dbConn req auth) >>= send200
          _ -> send404
      ["nutzer", "neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ \auth -> (User.Handlers.saveNewUser dbConn req auth) >>= send200
          "GET" -> adminOnly' (User.Handlers.showAddUserForm >=> send200)
          _ -> send404
      ["nutzer", int, "editieren"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
          Right (parsed :: Int) ->
            let userId = UserId parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnlyOrOwn userId $
                    \(id, auth) ->
                      (User.Handlers.showEditUserForm dbConn id auth)
                        >>= send200
                  "POST" -> adminOnlyOrOwn userId $
                    \(id, auth) ->
                      (User.Handlers.updateExistingUser dbConn req id auth)
                        >>= send200
                  _ -> send404
      ["nutzer", int] ->
        case Wai.requestMethod req of
          "GET" ->
            case readEither (Text.unpack int) of
              Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
              Right (parsed :: Int) -> authenticatedOnly' $ \auth -> (User.Handlers.showProfile dbConn parsed auth) >>= send200
          _ -> send404
      ["nutzer", int, "loeschen"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
          Right (parsed :: Int) ->
            let userId = UserId parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnly' $ \auth -> (User.Handlers.showDeleteConfirmation dbConn userId auth) >>= send200
                  "POST" -> adminOnly' $ \auth -> (User.Handlers.deleteUser dbConn userId auth) >>= send200
                  _ -> send404
      ["login"] ->
        case Wai.requestMethod req of
          "POST" -> Login.login dbConn userSalt signerKey saltSep sessionKey appEnv req send
          "GET" -> (Login.showLoginForm sessionDataVaultKey req) >>= send200
          _ -> send404
      ["logout"] ->
        case Wai.requestMethod req of
          "POST" -> Login.logout dbConn appEnv sessionDataVaultKey req send
          _ -> send404
      ["passwort", "aendern"] ->
        case Wai.requestMethod req of
          "GET" -> (PasswordReset.showChangePwForm req) >>= send200
          "POST" -> (PasswordReset.handleChangePw logger dbConn req) >>= send200
          _ -> send404
      ["passwort", "link"] ->
        case Wai.requestMethod req of
          "GET" -> (PasswordReset.showResetForm) >>= send200
          "POST" -> (PasswordReset.handleReset dbConn req sendMail') >>= send200
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
              p_ [class_ "alert alert-secondary", role_ "alert"] "Nicht Gefunden"

main :: IO ()
main = do
  -- TODO: Would be nicer to read all of this from a file
  sqlitePath <- getEnv "LIONS_SQLITE_PATH"
  appEnv <- getEnv "LIONS_ENV" >>= parseEnv
  sessionKeyFile <- getEnv "LIONS_SESSION_KEY_FILE"
  mailAwsAccessKey <- getEnv "LIONS_AWS_SES_ACCESS_KEY"
  mailAwsSecretAccessKey <- getEnv "LIONS_AWS_SES_SECRET_ACCESS_KEY"
  userSalt <- (encodeUtf8 . Text.pack) <$> getEnv "LIONS_SCRYPT_USER_SALT"
  signerKey <- (encodeUtf8 . Text.pack) <$> getEnv "LIONS_SCRYPT_SIGNER_KEY"
  saltSep <- (encodeUtf8 . Text.pack) <$> getEnv "LIONS_SCRYPT_SALT_SEP"
  sessionKey <- ClientSession.getKey sessionKeyFile
  sessionDataVaultKey <- Vault.newKey
  requestIdVaultKey <- Vault.newKey

  SQLite.withConnection
    sqlitePath
    ( \conn -> do
        let aKey = AWS.AccessKey (encodeUtf8 (Text.pack mailAwsAccessKey))
            sKey = AWS.SecretKey (encodeUtf8 (Text.pack mailAwsSecretAccessKey))
        awsEnv <- AWS.newEnv (AWS.FromKeys aKey sKey)

        formattedTime <- newTimeCache simpleTimeFormat
        withTimedFastLogger formattedTime (LogStdout defaultBufSize) $ \logger -> do
          let port = (3000 :: Int)
              app' =
                -- Yeah this is getting long. Maybe back to capability or smth else?
                server
                  logger
                  conn
                  sessionKey
                  requestIdVaultKey
                  awsEnv
                  port
                  appEnv
                  sessionDataVaultKey
                  userSalt
                  signerKey
                  saltSep

          SQLite.execute_ conn "PRAGMA foreign_keys"
          (runSettings . setPort port $ setHost "localhost" defaultSettings)
            . logStdout
            . staticPolicy (addBase "public")
            $ ( \req send ->
                  handleAny
                    ( \e -> do
                        Logging.log logger $ show e
                        send500 send
                    )
                    $ ( RequestID.Middleware.middleware requestIdVaultKey
                          . ( Session.middleware
                                logger
                                sessionDataVaultKey
                                conn
                                sessionKey
                            )
                      )
                      app'
                      req
                      send
              )
    )
  where
    send500 send = do
      send
        . Wai.responseLBS status500 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout "Fehler" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Es ist leider ein Fehler aufgetreten"
