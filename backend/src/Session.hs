module Session
  ( isUserAdmin,
    getSessionFromAuth,
    getAuthFromVault,
    createNewSession,
    deleteSessionsForUser,
    tryLogin,
    deleteSession,
    getSessionsFromDbByUser,
    saveSession,
  )
where

import Control.Exception.Safe
import qualified Crypto.BCrypt as BCrypt
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Network.Wai.Session (genSessionId)
import Session.Types
import Time (timeDaysFromNow)
import User.DB (getCredentials)
import User.Types (Role (..), UserId (..))
import Prelude hiding (id, log)

createNewSession :: UserId -> IO ValidSession
createNewSession uid = do
  expires <- timeDaysFromNow 30
  sessionid <- decodeUtf8 <$> genSessionId
  return . ValidSession $ Session (SessionId sessionid) expires uid

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
isUserAdmin (IsAdmin _) = True
isUserAdmin _ = False

getSessionFromAuth :: Authenticated -> UserSession
getSessionFromAuth (IsAdmin (AdminUser session)) = session
getSessionFromAuth (IsUser session) = session
