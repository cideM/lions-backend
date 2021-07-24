module Password.Password
  ( hash,
    update,
    save,
    Hashed (..),
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Crypto.BCrypt as BCrypt
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.SQLite.Simple as SQLite
import qualified Katip as K
import qualified Password.Reset.Token as Token
import qualified UnliftIO
import User.Types (UserId (..))
import Prelude hiding (id, log)

newtype Hashed = Hashed Text deriving (Show)

update ::
  ( MonadIO m,
    MonadReader env m,
    K.KatipContext m,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env,
    MonadThrow m
  ) =>
  UserId ->
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
  UserId ->
  m ()
save hashed (UserId userid) = do
  conn <- asks App.getDb
  let newPw = unhash hashed
  liftIO $ SQLite.execute conn "update users set password_digest = ? where id = ?" (newPw, show userid)
  where
    unhash (Hashed s) = s

hash ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  Text ->
  m Hashed
hash pw =
  liftIO $
    BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy (encodeUtf8 pw) >>= \case
      Nothing -> throwString "hashing password failed"
      Just pw' -> return . Hashed $ decodeUtf8 pw'
