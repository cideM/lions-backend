module Password.Password
  ( hash,
    update,
    get,
    save,
    Hashed (..),
  )
where

import qualified App
import Control.Exception.Safe
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Crypto.KDF.BCrypt (hashPassword)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.SQLite.Simple as SQLite
import qualified Katip as K
import qualified Password.Reset.Token as Token
import qualified UnliftIO
import qualified User.Id as User
import Prelude hiding (id, log)

newtype Hashed = Hashed Text

newtype Salt = Salt Text

-- There are two generations of users. Those imported from Firebase (SCrypt),
-- together with their hashed passwords and salts. And new users, who are
-- created with BCrypt.
data Password a
  = FirebaseUser Salt a
  | BCryptUser a

update ::
  ( MonadIO m,
    MonadReader env m,
    K.KatipContext m,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env,
    MonadThrow m
  ) =>
  User.Id ->
  Hashed ->
  m ()
update uid hashedPassword = do
  dbConn <- asks App.getDb
  UnliftIO.withRunInIO $ \runInIO ->
    SQLite.withTransaction
      dbConn
      ( runInIO $ do
          Token.delete uid
          save hashedPassword uid
      )

save ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env,
    MonadThrow m
  ) =>
  Hashed ->
  User.Id ->
  m ()
save hashed (User.Id userid) = do
  conn <- asks App.getDb
  let newPw = unhash hashed
  liftIO $ SQLite.execute conn "update users set password_digest = ? where id = ?" (newPw, show userid)
  -- If we don't set the salt, that salt will be used to decrypt the password with SCrypt,
  -- even though it was encrypted with BCrypt.
  liftIO $ SQLite.execute conn "update users set salt = null where id = ?" [(show userid)]
  where
    unhash (Hashed s) = s

hash ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  Text ->
  m Hashed
hash pw = do
  (hashed :: ByteString) <- liftIO $ hashPassword 12 (encodeUtf8 pw)
  return (Hashed $ decodeUtf8 hashed)

get ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env,
    MonadThrow m
  ) =>
  Text ->
  m (Maybe (Password Hashed))
get email = do
  conn <- asks App.getDb
  rows <- liftIO $ SQLite.query conn "SELECT salt, password_digest FROM users WHERE email = ? collate nocase" [email]
  return $ case rows of
    [(salt, pw)] ->
      if salt == ""
        then Just $ BCryptUser (Hashed pw)
        else Just $ FirebaseUser (Salt salt) (Hashed pw)
    [] -> Nothing
    _ -> throwString "unexpected result from DB for user id and password. not logging result, so please debug getCredentials"
