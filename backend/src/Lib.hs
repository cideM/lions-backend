module Lib (server, main) where

import Control.Exception.Safe
import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Env (Environment (..), parseEnv)
import Events.Domain (EventId (..))
import qualified Events.Handlers
import Layout (LayoutStub (..), layout)
import qualified Logging
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
import Session (SessionDataVaultKey)
import qualified Session
import System.Environment (getEnv)
import System.Log.FastLogger (LogType' (..), defaultBufSize, newTimeCache, simpleTimeFormat, withTimedFastLogger)
import Text.Read (readEither)
import User.Types (Role (..), UserId (..))
import qualified User.User
import qualified Userlist
import qualified Userprofile
import qualified Web.ClientSession as ClientSession
import WelcomeMessage (WelcomeMsgId (..))
import qualified WelcomeMessage
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
  signerKey
  saltSep
  req
  send = do
    let vault = Wai.vault req
        adminOnlyOrOwn id next = case routeData of
          Session.IsAuthenticated auth@(Session.IsAdmin _) -> next (id, auth)
          Session.IsAuthenticated auth@(Session.IsUser Session.UserSession {Session.userSessionUserId = userId}) ->
            if userId == id then next (id, auth) else send403
          _ -> send403
        adminOnly' next = case routeData of
          Session.IsAuthenticated (Session.IsAdmin auth) -> next auth
          _ -> send403
        authenticatedOnly' next = case routeData of
          Session.IsAuthenticated auth -> next auth
          _ -> send403
        routeData = case Vault.lookup sessionDataVaultKey vault of
          Nothing -> Session.IsNotAuthenticated
          Just (roles, userid) ->
            Session.IsAuthenticated $
              if Admin `elem` roles
                then Session.IsAdmin . Session.AdminUser $ Session.UserSession userid roles
                else Session.IsUser $ Session.UserSession userid roles
        resetHost =
          if appEnv == Production
            then "https://www.lions-achern.de"
            else Text.pack $ "http://localhost:" <> show port
        sendMail' = SendEmail.sendMail awsEnv resetHost
        layout' = layout routeData
        send200 =
          send
            . Wai.responseLBS status200 [("Content-Type", "text/html; charset=UTF-8")]
            . renderBS
        send403 =
          send
            . Wai.responseLBS status403 [("Content-Type", "text/html; charset=UTF-8")]
            . renderBS
            . layout'
            . LayoutStub "Fehler" Nothing
            $ div_ [class_ "container p-3 d-flex justify-content-center"] $
              div_ [class_ "row col-6"] $ do
                p_ [class_ "alert alert-secondary", role_ "alert"] "Du hast keinen Zugriff auf diese Seite"
        send404 =
          send
            . Wai.responseLBS status404 [("Content-Type", "text/html; charset=UTF-8")]
            . renderBS
            . layout'
            . LayoutStub "Nicht gefunden" Nothing
            $ div_ [class_ "container p-3 d-flex justify-content-center"] $
              div_ [class_ "row col-6"] $ do
                p_ [class_ "alert alert-secondary", role_ "alert"] "Nicht Gefunden"
    case Wai.pathInfo req of
      [] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ WelcomeMessage.showFeed dbConn >=> send200 . layout'
          _ -> send404
      ["veranstaltungen"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ Events.Handlers.showAllEvents dbConn >=> send200 . layout'
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", "neu"] ->
        case Wai.requestMethod req of
          "GET" -> adminOnly' $ Events.Handlers.showCreateEvent >=> send200 . layout'
          "POST" -> adminOnly' $ Events.Handlers.handleCreateEvent dbConn req >=> send200 . layout'
          -- TODO: Send unsupported method 405
          _ -> send404
      ["veranstaltungen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" ->
                authenticatedOnly' $
                  Events.Handlers.showEvent dbConn (EventId parsed) >=> \case
                    Nothing -> send404
                    Just stub -> send200 $ layout' stub
              _ -> send404
      ["veranstaltungen", i, "antwort"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "POST" -> authenticatedOnly' $ Events.Handlers.replyToEvent dbConn req send (EventId parsed)
              _ -> send404
      ["veranstaltungen", i, "loeschen"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ Events.Handlers.showDeleteEventConfirmation dbConn (EventId parsed) >=> send200 . layout'
              "POST" -> adminOnly' $ Events.Handlers.handleDeleteEvent dbConn (EventId parsed) >=> send200 . layout'
              _ -> send404
      ["veranstaltungen", i, "editieren"] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for event ID as int: " <> i
          Right (parsed :: Int) ->
            case Wai.requestMethod req of
              "GET" -> adminOnly' $ Events.Handlers.showEditEventForm dbConn (EventId parsed) >=> send200 . layout'
              "POST" -> adminOnly' $ Events.Handlers.handleUpdateEvent dbConn req (EventId parsed) >=> send200 . layout'
              _ -> send404
      ["loeschen", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = WelcomeMsgId parsed
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ WelcomeMessage.handleDeleteMessage dbConn msgId >=> send200 . layout'
                  "GET" -> adminOnly' $ WelcomeMessage.showDeleteConfirmation dbConn msgId >=> send200 . layout'
                  _ -> send404
      ["neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ WelcomeMessage.saveNewMessage dbConn req >=> send200 . layout'
          "GET" -> adminOnly' $ WelcomeMessage.showAddMessageForm >=> send200 . layout'
          _ -> send404
      ["editieren", i] ->
        case readEither (Text.unpack i) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for welcome message ID as int: " <> i
          Right (parsed :: Int) ->
            let msgId = WelcomeMsgId parsed
             in case Wai.requestMethod req of
                  "POST" -> adminOnly' $ WelcomeMessage.handleEditMessage dbConn req msgId >=> send200 . layout'
                  "GET" -> adminOnly' $ WelcomeMessage.showMessageEditForm dbConn msgId >=> send200 . layout'
                  _ -> send404
      ["nutzer"] ->
        case Wai.requestMethod req of
          "GET" -> authenticatedOnly' $ Userlist.get dbConn req >=> send200 . layout'
          _ -> send404
      ["nutzer", "neu"] ->
        case Wai.requestMethod req of
          "POST" -> adminOnly' $ User.User.createPost dbConn req >=> send200 . layout'
          "GET" -> adminOnly' (User.User.createGet >=> send200 . layout')
          _ -> send404
      ["nutzer", int, "editieren"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
          Right (parsed :: Int) ->
            let userId = UserId parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnlyOrOwn userId $
                    \(id, auth) ->
                      User.User.editGet dbConn id auth
                        >>= send200 . layout'
                  "POST" -> adminOnlyOrOwn userId $
                    \(id, auth) ->
                      User.User.editPost dbConn req id auth
                        >>= send200 . layout'
                  _ -> send404
      ["nutzer", int] ->
        case Wai.requestMethod req of
          "GET" ->
            case readEither (Text.unpack int) of
              Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
              Right (parsed :: Int) ->
                authenticatedOnly' $
                  Userprofile.get dbConn parsed >=> \case
                    Nothing -> send404
                    Just stub -> send200 $ layout' stub
          _ -> send404
      ["nutzer", int, "loeschen"] ->
        case readEither (Text.unpack int) of
          Left _ -> throwString . Text.unpack $ "couldn't parse route param for UserId as int: " <> int
          Right (parsed :: Int) ->
            let userId = UserId parsed
             in case Wai.requestMethod req of
                  "GET" -> adminOnly' $ User.User.deleteGet dbConn userId >=> send200 . layout'
                  "POST" -> adminOnly' $ User.User.deletePost dbConn userId >=> send200 . layout'
                  _ -> send404
      ["login"] ->
        case Wai.requestMethod req of
          "POST" -> Login.login dbConn logger signerKey saltSep sessionKey appEnv req send
          "GET" -> Login.showLoginForm routeData >>= send200
          _ -> send404
      ["logout"] ->
        case Wai.requestMethod req of
          "POST" -> Login.logout dbConn appEnv sessionDataVaultKey req send
          _ -> send404
      ["passwort", "aendern"] ->
        case Wai.requestMethod req of
          "GET" -> PasswordReset.showChangePwForm req >>= send200 . layout'
          "POST" -> PasswordReset.handleChangePw logger dbConn req >>= send200 . layout'
          _ -> send404
      ["passwort", "link"] ->
        case Wai.requestMethod req of
          "GET" -> PasswordReset.showResetForm >>= send200 . layout'
          "POST" -> PasswordReset.handleReset dbConn req sendMail' >>= send200 . layout'
          _ -> send404
      _ -> send404

type RequestIdVaultKey = Vault.Key UUID

addRequestId :: RequestIdVaultKey -> Wai.Application -> Wai.Application
addRequestId requestIdVaultKey next req send = do
  uuid <- nextRandom
  let vault' = Vault.insert requestIdVaultKey uuid (Wai.vault req)
      req' = req {Wai.vault = vault'}
  next req' send

main :: IO ()
main = do
  -- TODO: Would be nicer to read all of this from a file
  -- 1. Create new S3 user
  -- 2. Create S3 bucket, no CF all users come from same town basically speed doesn't matter
  -- 3. Add keys everywhere
  -- 4..Add form field multiple files
  -- 5. Figure out how to use those form fields... it'll suck because
  --    validation comes from the server and now you need to wait for
  --    validation until your files are also transmitted... but then again
  --    those PDFs shouldn't be too big or whatever else it is
  -- 6. Upload files to S3 and insert into table event_attachments eventid fileurl or path on S3
  -- 7. When someone looks at one of those events, just generate presigned URL
  --    for all files in bucket and display that as link
  -- 8. Display remove icon next to each file in event which is a DELETE requets
  sqlitePath <- getEnv "LIONS_SQLITE_PATH"
  appEnv <- getEnv "LIONS_ENV" >>= parseEnv
  sessionKeyFile <- getEnv "LIONS_SESSION_KEY_FILE"
  mailAwsAccessKey <- getEnv "LIONS_AWS_SES_ACCESS_KEY"
  mailAwsSecretAccessKey <- getEnv "LIONS_AWS_SES_SECRET_ACCESS_KEY"
  signerKey <- encodeUtf8 . Text.pack <$> getEnv "LIONS_SCRYPT_SIGNER_KEY"
  saltSep <- encodeUtf8 . Text.pack <$> getEnv "LIONS_SCRYPT_SALT_SEP"
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
                    $ (addRequestId requestIdVaultKey . Session.middleware logger sessionDataVaultKey conn sessionKey)
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
        . layout Session.IsNotAuthenticated
        . LayoutStub "Fehler" Nothing
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-secondary", role_ "alert"] "Es ist leider ein Fehler aufgetreten"
