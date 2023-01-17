module Session.Middleware (middleware) where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Error as E
import qualified Katip as K
import Network.HTTP.Types (status302)
import qualified Network.Wai as Wai
import Session.Session (Session (..))
import qualified Session.Session as Session
import qualified Session.Valid
import qualified User.Role.DB as Role
import qualified Wai
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
  (E.runExceptT $ login req) >>= \case
    Left e -> do
      case e of
        Nothing -> return ()
        Just err -> K.logLocM K.InfoS $ K.ls err

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

login ::
  ( MonadIO m,
    E.MonadError (Maybe Text) m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env,
    App.HasSessionEncryptionKey env,
    App.HasSessionDataVaultKey env
  ) =>
  Wai.Request ->
  m Wai.Request
login req = do
  sessionDataVaultKey <- asks App.getSessionDataVaultKey
  sessionId <- getSessionId
  session@(Session _ _ userId) <- Session.get sessionId >>= E.note' (Just "no session found")
  tryParse <- Session.Valid.parse session
  case tryParse of
    Left err -> throwString $ T.unpack err
    Right _ -> do
      roles <- Role.get userId >>= E.note' (Just "no roles found")
      let vault' = Vault.insert sessionDataVaultKey (roles, userId) $ Wai.vault req
      return $ req {Wai.vault = vault'}
  where
    getSessionId = do
      sessionKey <- asks App.getSessionEncryptionKey
      cookie <- E.note' Nothing . lookup "cookie" $ Wai.requestHeaders req
      session <- E.note' Nothing . lookup "lions_session" $ Cookie.parseCookies cookie
      E.when (session == "") (E.liftEither $ Left Nothing)
      decrypted <- E.note' (Just "empty session cookie") $ ClientSession.decrypt sessionKey session
      return . Session.Id $ decodeUtf8 decrypted
