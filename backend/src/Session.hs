module Session
  ( middleware,
    Session (..),
    isUserAdmin,
    getSessionFromAuth,
    getAuthFromVault,
    SessionDataVaultKey,
    SessionId (..),
    ValidSession (..),
    createNewSession,
    deleteSessionsForUser,
    tryLogin,
    deleteSession,
    getSessionsFromDbByUser,
    saveSession,
    UserSession (..),
    Authentication (..),
    AdminUser (..),
    Authenticated (..),
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BCrypt
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import qualified Logging
import Network.HTTP.Types (status302)
import qualified Network.Wai as Wai
import Network.Wai.Session (genSessionId)
import qualified Network.Wai.Trans as WaiT
import Time (timeDaysFromNow)
import User.DB
  ( getCredentials,
    getRolesFromDb,
  )
import User.Types (Role (..), UserId (..))
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id, log)

data UserSession = UserSession
  { userSessionUserId :: UserId,
    userSessionUserRoles :: [Role]
  }
  deriving (Show, Eq)

data Authentication = IsNotAuthenticated | IsAuthenticated Authenticated deriving (Show, Eq)

data Authenticated = IsUser UserSession | IsAdmin AdminUser deriving (Show, Eq)

newtype AdminUser = AdminUser UserSession deriving (Show, Eq)

data Session = Session SessionId Time.UTCTime UserId deriving (Show)

type SessionDataVaultKey = Vault.Key ([Role], UserId)

newtype SessionId = SessionId Text
  deriving (Show, Eq)

-- A wrapper around a potentially invalid session so that I can differentiate
-- the two possible session types through types. I should not export the constructor actually.
-- TODO: Hide constructor
newtype ValidSession = ValidSession Session deriving (Show)

createNewSession :: UserId -> IO ValidSession
createNewSession uid = do
  expires <- timeDaysFromNow 30
  sessionid <- decodeUtf8 <$> genSessionId
  return . ValidSession $ Session (SessionId sessionid) expires uid

makeValidSession :: Session -> IO (Either Text ValidSession)
makeValidSession s@(Session _ expires _) = do
  now <- Time.getCurrentTime
  if now >= expires
    then return . Left $ "session expired at: " <> Text.pack (show expires)
    else return . pure $ ValidSession s

getSessionFromDb :: SQLite.Connection -> SessionId -> IO (Maybe Session)
getSessionFromDb conn (SessionId id) =
  SQLite.query conn "SELECT key,expires,userid FROM sessions WHERE key = ?" [id]
    >>= \case
      [(key, expires, userid) :: (Text, Time.UTCTime, Int)] ->
        return . Just $ Session (SessionId key) expires (UserId userid)
      [] -> return Nothing
      other -> throwString $ "unexpected DB result for session: " <> show other

getSessionsFromDbByUser :: SQLite.Connection -> UserId -> IO [Session]
getSessionsFromDbByUser conn (UserId id) =
  SQLite.query conn "SELECT key,expires,userid FROM sessions WHERE userid = ?" [id]
    >>= \case
      (sessions :: [(Text, Time.UTCTime, Int)]) ->
        return $ map (\(key, expires, userid) -> Session (SessionId key) expires (UserId userid)) sessions

deleteSessionsForUser :: SQLite.Connection -> UserId -> IO ()
deleteSessionsForUser conn (UserId uid) = SQLite.execute conn "delete from sessions where userid = ?" [uid]

saveSession :: SQLite.Connection -> ValidSession -> IO ()
saveSession conn (ValidSession (Session (SessionId key) expires (UserId uid))) =
  SQLite.execute conn "INSERT INTO sessions (key,expires,userid) VALUES (?,?,?)" (key, expires, uid)

deleteSession :: SQLite.Connection -> Session -> IO ()
deleteSession conn (Session (SessionId id) _ _) =
  SQLite.execute conn "DELETE FROM sessions WHERE key = ?" $ SQLite.Only id

getSessionIdFromReq :: ClientSession.Key -> Wai.Request -> Either Text SessionId
getSessionIdFromReq sessionKey req =
  case lookup "cookie" $ Wai.requestHeaders req of
    Nothing -> Left "no cookie header"
    Just cookie -> case lookup "lions_session" $ Cookie.parseCookies cookie of
      Nothing -> Left "no session cookie"
      Just session ->
        case ClientSession.decrypt sessionKey session of
          Nothing -> Left "empty session cookie"
          Just decrypted -> Right . SessionId $ decodeUtf8 decrypted

-- Try to authenticate a user based on the session ID from the client cookie.
tryLoginFromSession ::
  SQLite.Connection ->
  ClientSession.Key ->
  SessionDataVaultKey ->
  Wai.Request ->
  IO (Either Text Wai.Request)
tryLoginFromSession dbConn sessionKey sessionDataVaultKey req = do
  case getSessionIdFromReq sessionKey req of
    Left err -> return $ Left err
    Right sessionId -> do
      getSessionFromDb dbConn sessionId >>= \case
        Nothing -> return $ Left "no session found"
        Just session@(Session _ _ userId) -> do
          makeValidSession session >>= \case
            Left e -> return $ Left e
            Right _ -> do
              getRolesFromDb dbConn userId >>= \case
                Nothing -> return $ Left "no roles found"
                Just roles -> do
                  let vault = Wai.vault req
                      vault' = Vault.insert sessionDataVaultKey (roles, userId) vault
                      req' = req {Wai.vault = vault'}
                  return $ Right req'

-- This is the main user facing export. This middleware will try to log a user
-- in based on cookie information. The login information is then persisted in
-- the Vault associated with this particular request so that downstream code
-- can access it.
middleware ::
  (MonadIO m) =>
  -- Logging.Log ->
  SessionDataVaultKey ->
  SQLite.Connection ->
  ClientSession.Key ->
  WaiT.MiddlewareT m
middleware sessionDataVaultKey dbConn sessionKey nextApp req send = do
  (liftIO $ tryLoginFromSession dbConn sessionKey sessionDataVaultKey req) >>= \case
    Left e -> do
      -- liftIO . log $ Text.pack $ "error in tryLoginFromSession: " <> show e
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

-- Check if user salt, existing hash and password match
type VerifyHash = (ByteString -> ByteString -> ByteString -> Either Text Bool)

-- Encrypt a ByteString for storage on the client
type ClientEncrypt = (ByteString -> IO ByteString)

-- Checks if credentials are valid and then generates a new session, stores it
-- in the DB and returns it to the caller. This function tries two different
-- hashing algorithms. One for old password hashes exported from Firebase and
-- the other is a standard BCrypt algorithm for any new passwords.
tryLogin ::
  SQLite.Connection ->
  VerifyHash ->
  ClientEncrypt ->
  Text ->
  Text ->
  IO (Either Text (ByteString, Time.UTCTime))
tryLogin
  conn
  verifyPassword
  clientEncrypt
  email
  formPw = do
    getCredentials conn email >>= \case
      Nothing -> return $ Left "no user found"
      Just (userId, userSalt, dbPw) -> do
        let formPw' = encodeUtf8 formPw
        let dbPw' = encodeUtf8 dbPw
        case userSalt of
          -- Firebase credentials
          Just us -> do
            case verifyPassword (encodeUtf8 us) dbPw' formPw' of
              Left e -> throwString [i|error trying to verify firebase pw: #{e}|]
              Right ok ->
                if ok
                  then onSuccess userId
                  else (return $ Left "incorrect password")
          -- BCrypt, new credentials
          Nothing -> do
            if not $ BCrypt.validatePassword dbPw' formPw'
              then return $ Left "incorrect password"
              else onSuccess userId
    where
      onSuccess userId = do
        newSession@(ValidSession (Session (SessionId sessionId) expires _)) <- createNewSession userId
        saveSession conn newSession
        encryptedSessionId <- clientEncrypt (encodeUtf8 sessionId)
        return $ Right (encryptedSessionId, expires)

getAuthFromVault :: Vault.Key ([Role], UserId) -> Vault.Vault -> Authentication
getAuthFromVault sessionDataVaultKey vault =
  case Vault.lookup sessionDataVaultKey vault of
    Nothing -> IsNotAuthenticated
    Just (roles, userid) ->
      IsAuthenticated $
        if Admin `elem` roles
          then IsAdmin . AdminUser $ UserSession userid roles
          else IsUser $ UserSession userid roles

isUserAdmin :: Authenticated -> Bool
isUserAdmin (Session.IsAdmin _) = True
isUserAdmin _ = False

getSessionFromAuth :: Authenticated -> UserSession
getSessionFromAuth (Session.IsAdmin (Session.AdminUser session)) = session
getSessionFromAuth (Session.IsUser session) = session
