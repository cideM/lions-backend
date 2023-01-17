module Password.Reset.Token
  ( delete,
    update,
    parse,
    ParseError (..),
    Valid (..),
    TokenId (..),
    TokenCreate (..),
    Token (..),
  )
where

import qualified App
import Control.Error hiding (tryIO, tryJust)
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader.Class (MonadReader, asks)
import Crypto.KDF.BCrypt (hashPassword)
import qualified Crypto.Random
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Time (timeDaysFromNow)
import qualified UnliftIO
import qualified User.Id as User
import qualified User.User as User
import Prelude hiding (id)

newtype TokenId = TokenId Int
  deriving (Show, Eq)

data TokenCreate = TokenCreate
  { tokenCreateValue :: Text,
    tokenCreateExpires :: Time.UTCTime,
    tokenCreateUserId :: User.Id
  }
  deriving (Show, Eq)

data Token = Token
  { tokenValue :: Text,
    tokenExpires :: Time.UTCTime,
    tokenId :: TokenId,
    tokenUserId :: User.Id
  }
  deriving (Show, Eq)

newtype Valid = Valid Token

get ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env,
    MonadThrow m
  ) =>
  Text ->
  m (Maybe Token)
get t = do
  conn <- asks App.getDb
  rows <- liftIO $ SQLite.query conn "select token, expires, id, userid from reset_tokens where token = ?" [t]
  case rows of
    [] -> return Nothing
    [(value, expires, id, userid) :: (Text, Time.UTCTime, Int, Int)] ->
      return . Just $ Token value expires (TokenId id) (User.Id userid)
    _ -> throwString . T.unpack $ "returned more than one token for value: " <> t

delete ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env,
    MonadThrow m
  ) =>
  User.Id ->
  m ()
delete (User.Id userid) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "delete from reset_tokens where userid = ?" $ SQLite.Only userid

save ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env,
    MonadThrow m
  ) =>
  TokenCreate ->
  m ()
save TokenCreate {tokenCreateUserId = (User.Id userid), ..} = do
  conn <- asks App.getDb
  liftIO $
    SQLite.execute
      conn
      "insert into reset_tokens (token, expires, userid) values (?,?,?)"
      (tokenCreateValue, tokenCreateExpires, userid)

-- Generate a new password reset token and its expiration date
create ::
  ( MonadThrow m,
    MonadIO m,
    Monad m
  ) =>
  m (Text, Time.UTCTime)
create = do
  expires <- liftIO $ timeDaysFromNow 10
  (token :: ByteString) <- liftIO $ do Crypto.Random.getRandomBytes 20
  (hashed :: ByteString) <- liftIO $ hashPassword 12 token
  return (decodeUtf8 hashed, expires)

-- Generates a new password reset token for the given user email and stores it
-- in the DB, removing whatever old tokens existed
update ::
  ( MonadIO m,
    MonadReader env m,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env,
    MonadThrow m
  ) =>
  Text ->
  m (Maybe Text)
update email = do
  conn <- asks App.getDb
  User.getCredentials email >>= \case
    Nothing -> return Nothing
    Just (userid, _, _) -> do
      (tokenValue, expires) <- create
      let token = TokenCreate tokenValue expires userid
      UnliftIO.withRunInIO $ \runInIO ->
        SQLite.withTransaction
          conn
          ( runInIO $ do
              delete userid
              save token
          )
      return $ Just tokenValue

data ParseError
  = NotFound Text
  | NoUser User.Id
  | Expired Token
  deriving (Show)

parse ::
  ( MonadIO m,
    MonadReader env m,
    MonadError ParseError m,
    App.HasDb env,
    MonadThrow m
  ) =>
  Text ->
  m Valid
parse value = do
  tok@Token {..} <- get value >>= liftEither . note (NotFound value)
  ok <- User.exists tokenUserId
  unless ok $ throwError (NoUser tokenUserId)
  now <- liftIO $ Time.getCurrentTime
  when (now >= tokenExpires) (throwError $ Expired tok)
  return $ Valid tok
