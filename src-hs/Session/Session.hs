module Session.Session
  ( middleware,
    Session (..),
    SessionDataVaultKey,
    SessionId (..),
    ValidSession (..),
    createNewSession,
    deleteSession,
    getSessionsFromDbByUser,
    saveSession,
  )
where

import Control.Exception.Safe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import qualified Logging.Logging as Logging
import Network.HTTP.Types (status302)
import qualified Network.Wai as Wai
import Network.Wai.Session (genSessionId)
import Time.Time (timeDaysFromNow)
import User.DB
  ( getRolesFromDb,
  )
import User.Domain (Role, UserId (..))
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

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

-- The business logic that powers the middleware below.
tryLogin ::
  SQLite.Connection ->
  ClientSession.Key ->
  SessionDataVaultKey ->
  Wai.Request ->
  IO (Either Text Wai.Request)
tryLogin dbConn sessionKey sessionDataVaultKey req = do
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
  Logging.TimedFastLogger ->
  SessionDataVaultKey ->
  SQLite.Connection ->
  ClientSession.Key ->
  Wai.Application ->
  Wai.Application
middleware _ sessionDataVaultKey dbConn sessionKey nextApp req send = do
  tryLogin dbConn sessionKey sessionDataVaultKey req >>= \case
    Left _ -> do
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
