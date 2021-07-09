module Session.Middleware (middleware) where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Error as E
import qualified Katip as K
import Session.Types
import Network.HTTP.Types (status302)
import qualified Network.Wai as Wai
import qualified Session.DB
import User.DB (getRolesFromDb)
import qualified Wai.Class as Wai
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id, log)

-- This is the main user facing export. This middleware will try to log a user
-- in based on cookie information. The login information is then persisted in
-- the Vault associated with this particular request so that downstream code
-- can access it.
middleware ::
  ( MonadIO m,
    K.KatipContext m,
    App.HasSessionDataVaultKey env,
    App.HasDb env,
    MonadThrow m,
    App.HasSessionEncryptionKey env,
    MonadReader env m
  ) =>
  Wai.MiddlewareT m
middleware nextApp req send = do
  tryLoginFromSession req >>= \case
    Left e -> do
      K.logLocM K.InfoS $ K.ls e
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

getSessionIdFromReq :: ClientSession.Key -> Wai.Request -> Either Text SessionId
getSessionIdFromReq sessionKey req = do
  cookie <- E.note "no cookie header" . lookup "cookie" $ Wai.requestHeaders req
  session <- E.note "no session cookie" . lookup "lions_session" $ Cookie.parseCookies cookie
  decrypted <- E.note "empty session cookie" $ ClientSession.decrypt sessionKey session
  Right . SessionId $ decodeUtf8 decrypted

makeValidSession :: Session -> IO (Either Text ValidSession)
makeValidSession s@(Session _ expires _) = do
  now <- Time.getCurrentTime
  if now >= expires
    then return . Left $ "session expired at: " <> Text.pack (show expires)
    else return . pure $ ValidSession s

-- TODO: Improve
tryLoginFromSession ::
  ( MonadIO m,
    -- E.MonadError Text m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env,
    App.HasSessionEncryptionKey env,
    App.HasSessionDataVaultKey env
  ) =>
  Wai.Request ->
  m (Either Text Wai.Request)
tryLoginFromSession req = do
  dbConn <- asks App.getDb
  sessionKey <- asks App.getSessionEncryptionKey
  sessionDataVaultKey <- asks App.getSessionDataVaultKey

  case getSessionIdFromReq sessionKey req of
    Left err -> return $ Left err
    Right sessionId -> do
      Session.DB.get sessionId >>= \case
        Nothing -> return $ Left "no session found"
        Just session@(Session _ _ userId) -> do
          (liftIO $ makeValidSession session) >>= \case
            Left e -> return $ Left e
            Right _ -> do
              (liftIO $ getRolesFromDb dbConn userId) >>= \case
                Nothing -> return $ Left "no roles found"
                Just roles -> do
                  let vault = Wai.vault req
                      vault' = Vault.insert sessionDataVaultKey (roles, userId) vault
                      req' = req {Wai.vault = vault'}
                  return $ Right req'
