module PasswordReset.Token
  ( get,
    delete,
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
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Crypto.BCrypt as BCrypt
import Crypto.Random (SystemRandom, genBytes, newGenIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import qualified Error as E
import Time (timeDaysFromNow)
import qualified UnliftIO
import User.DB (getCredentials)
import qualified User.DB
import User.Types (UserId (..))
import Prelude hiding (id)

newtype TokenId = TokenId Int
  deriving (Show, Eq)

data TokenCreate = TokenCreate
  { tokenCreateValue :: Text,
    tokenCreateExpires :: Time.UTCTime,
    tokenCreateUserId :: UserId
  }
  deriving (Show, Eq)

data Token = Token
  { tokenValue :: Text,
    tokenExpires :: Time.UTCTime,
    tokenId :: TokenId,
    tokenUserId :: UserId
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
      return . Just $ Token value expires (TokenId id) (UserId userid)
    _ -> throwString . T.unpack $ "returned more than one token for value: " <> t

delete ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env,
    MonadThrow m
  ) =>
  UserId ->
  m ()
delete (UserId userid) = do
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
save TokenCreate {tokenCreateUserId = (UserId userid), ..} = do
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
  (g :: SystemRandom) <- liftIO $ newGenIO
  token <- case genBytes 20 g of
    Left e -> throwString $ show e
    Right (token', _) ->
      (liftIO $ BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy token') >>= \case
        Nothing -> throwString "hashing reset token failed"
        Just token'' -> return $ decodeUtf8 token''
  return (token, expires)

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
  (liftIO $ getCredentials conn email) >>= \case
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
  | NoUser UserId
  | Expired Token
  deriving (Show)

parse ::
  ( MonadIO m,
    MonadReader env m,
    E.MonadError ParseError m,
    App.HasDb env,
    MonadThrow m
  ) =>
  Text ->
  m Valid
parse value = do
  dbConn <- asks App.getDb
  tok@Token {..} <- get value >>= E.note' (NotFound value)
  ok <- liftIO $ User.DB.hasUser dbConn tokenUserId
  E.unless ok $ E.throwError (NoUser tokenUserId)
  now <- liftIO $ Time.getCurrentTime
  E.when (now >= tokenExpires) (E.throwError $ Expired tok)
  return $ Valid tok
