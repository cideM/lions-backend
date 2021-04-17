{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module User.DB
  ( deleteUserById,
    getUser,
    getUsers,
    getIdAndPwByEmail,
    saveUser,
    getRolesFromDb,
    saveUserRoles,
    updateUser,
    DBEmail(..),
  )
where

import Control.Exception.Safe
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BCrypt
import Crypto.Random (SystemRandom, genBytes, newGenIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromField (FromField (..), ResultError (..), fieldData, returnError)
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow (..))
import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as Email
import TextShow
import User.Domain (Role (..), UserId (..), UserEmail(..), UserProfile (..), parseRole)
import Prelude hiding (id)

newtype DBRole = DBRole Role
  deriving (Show) via Role

instance ToField DBRole where
  toField = toField . show

instance FromField DBRole where
  fromField f =
    case fieldData f of
      (SQLite.SQLText "admin") -> Ok $ DBRole Admin
      (SQLite.SQLText "board") -> Ok $ DBRole Board
      (SQLite.SQLText "president") -> Ok $ DBRole Board
      (SQLite.SQLText "user") -> Ok $ DBRole Board
      _ -> returnError ConversionFailed f "unknown user role"

newtype DBUserId = DBUserId UserId
  deriving (Show)
  deriving (TextShow) via Int
  deriving (FromField) via Int
  deriving (ToField) via Int
  deriving (ToRow) via (SQLite.Only Int)

newtype UserProfileWithPw = UserProfileWithPw (Text, UserProfile)

instance ToRow UserProfileWithPw where
  toRow (UserProfileWithPw (pw, profile)) = toField pw : toRow (DBUserProfile profile)

newtype DBEmail = DBEmail EmailAddress deriving Show

instance FromField DBEmail where
  fromField f =
    ( case fieldData f of
        (SQLite.SQLBlob v) -> Ok v
        (SQLite.SQLText v) -> Ok $ encodeUtf8 v
        _ -> returnError ConversionFailed f "unexpected field type for email"
    )
      >>= \email -> case Email.emailAddress email of
        Nothing -> returnError ConversionFailed f ("couldn't parse email from DB: " <> show email)
        Just parsed -> Ok $ DBEmail parsed

newtype DBUserProfile = DBUserProfile UserProfile deriving (Show)

instance ToRow DBUserProfile where
  toRow
    ( DBUserProfile
        ( UserProfile
            (UserEmail _userEmail)
            _userFirstName
            _userLastName
            _userAddress
            _userMobilePhoneNr
            _userLandlineNr
            _userBirthday
            _userFirstNamePartner
            _userLastNamePartner
            _userBirthdayPartner
          )
      ) =
      [ toField (Email.toByteString _userEmail),
        toField _userFirstName,
        toField _userLastName,
        toField _userAddress,
        toField _userMobilePhoneNr,
        toField _userLandlineNr,
        toField _userBirthday,
        toField _userFirstNamePartner,
        toField _userLastNamePartner,
        toField _userBirthdayPartner
      ]

newtype UserIdAndProfile = UserIdAndProfile (DBUserId, DBUserProfile)
  deriving (Show)

-- Profile and ID are flipped!
instance ToRow UserIdAndProfile where
  toRow (UserIdAndProfile (userid, profile)) = toRow profile ++ toRow userid

newtype RoleAndProfile = RoleAndProfile (Text, DBUserId, UserProfile)
  deriving (Show)

instance FromRow RoleAndProfile where
  fromRow = do
    rolesCommaSep <- SQLite.field
    userId <- SQLite.field
    (DBEmail email) <- SQLite.field
    f3 <- SQLite.field
    f4 <- SQLite.field
    f5 <- SQLite.field
    f6 <- SQLite.field
    f7 <- SQLite.field
    f8 <- SQLite.field
    f9 <- SQLite.field
    f10 <- SQLite.field
    f11 <- SQLite.field
    return $ RoleAndProfile (rolesCommaSep, userId, UserProfile (UserEmail email) f3 f4 f5 f6 f7 f8 f9 f10 f11)

-- I've spent about 2h debugging this GROUP BY NULL shit, fml
-- https://stackoverflow.com/questions/3652580/how-to-prevent-group-concat-from-creating-a-result-when-no-input-data-is-present
getUser :: (MonadIO m, MonadThrow m) => SQLite.Connection -> UserId -> m (Maybe ([Role], (UserId, UserProfile)))
getUser conn userid =
  do
    liftIO
      ( SQLite.query
          conn
          [sql|
           SELECT GROUP_CONCAT(label,","), users.id, email, first_name, last_name, address, mobile_phone_nr, landline_nr,
                  birthday, first_name_partner, last_name_partner, birthday_partner
           FROM users
           JOIN user_roles ON users.id = userid
           JOIN roles ON roles.id = roleid
           WHERE users.id = ?
           GROUP BY NULL
          |]
          (SQLite.Only (DBUserId userid))
      )
    >>= \case
      [] -> return Nothing
      [v :: RoleAndProfile] ->
        case makeProfile v of
          Left e -> throwString $ Text.unpack ("couldn't get user: " <> e)
          Right v' -> return $ Just v'
      result -> throwString $ "got unexpected getUser result" <> show result

-- TODO: This stuff here is something I could test
makeProfile :: RoleAndProfile -> Either Text ([Role], (UserId, UserProfile))
makeProfile (RoleAndProfile (roles, DBUserId userid, profile)) = do
  roles' <- traverse parseRole (Text.splitOn "," roles)
  pure (roles', (userid, profile))

deleteUserById :: (MonadIO m) => SQLite.Connection -> UserId -> m ()
deleteUserById conn userid = do
  liftIO $ SQLite.execute conn "DELETE FROM users WHERE id = ?" . SQLite.Only $ DBUserId userid
  liftIO $ SQLite.execute conn "DELETE FROM user_roles WHERE userid = ?" . SQLite.Only $ DBUserId userid

getUsers :: (MonadIO m, MonadCatch m, MonadThrow m) => SQLite.Connection -> m [([Role], (UserId, UserProfile))]
getUsers conn =
  handleAny
    (\e -> throwString $ "error getting users: " <> show e)
    ( liftIO
        ( SQLite.query_
            conn
            [sql|
           SELECT GROUP_CONCAT(label,","), users.id, email, first_name, last_name, address, mobile_phone_nr, landline_nr,
                  birthday, first_name_partner, last_name_partner, birthday_partner
           FROM users
           JOIN user_roles ON users.id = userid
           JOIN roles ON roles.id = roleid
           GROUP BY users.id
          |]
        )
        >>= \values ->
          case traverse makeProfile values of
            Left e -> throwString $ "error making profile: " <> Text.unpack e
            Right v -> pure v
    )

-- TODO: Email type
getIdAndPwByEmail :: (MonadIO m) => SQLite.Connection -> Text -> m (Maybe (UserId, Text))
getIdAndPwByEmail conn email = do
  r <- liftIO $ SQLite.query conn "SELECT id, password_digest FROM users WHERE email = ?" [email]
  return $ case r of
    [(DBUserId userid, pw)] -> Just (userid, pw)
    [] -> Nothing
    other -> throwString $ "unexpected result from DB for password" <> show other

updateUser :: SQLite.Connection -> UserId -> [Role] -> UserProfile -> IO ()
updateUser conn userid roles profile = do
  SQLite.execute conn "DELETE FROM user_roles WHERE userid = ?" (SQLite.Only (DBUserId userid))
  saveUserRoles conn userid roles
  SQLite.execute
    conn
    [sql|
    UPDATE users
    SET email = ?
    ,   first_name = ?
    ,   last_name = ?
    ,   address = ?
    ,   mobile_phone_nr = ?
    ,   landline_nr  = ?
    ,   birthday  = ?
    ,   first_name_partner  = ?
    ,   last_name_partner  = ?
    ,   birthday_partner = ?
    WHERE id = ?
    |]
    $ UserIdAndProfile (DBUserId userid, DBUserProfile profile)

saveUser :: SQLite.Connection -> UserProfile -> IO ()
saveUser conn profile = do
  g <- newGenIO :: IO SystemRandom
  password <- case genBytes 20 g of
    Left e -> throwString $ show e
    Right (pw, _) -> return pw
  hashed <-
    BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy password >>= \case
      Nothing -> throwString "hashing password failed"
      Just pw -> return $ decodeUtf8 pw
  SQLite.execute
    conn
    [sql|
    INSERT INTO users (
      password_digest,
      email,
      first_name,
      last_name,
      address,
      mobile_phone_nr,
      landline_nr ,
      birthday ,
      first_name_partner ,
      last_name_partner ,
      birthday_partner 
    ) VALUES (?, ?, ?, ?, ?, ?, ? , ? , ? , ? , ?)
    |]
    $ UserProfileWithPw (hashed, profile)

getRolesFromDb :: (MonadIO m, MonadThrow m) => SQLite.Connection -> UserId -> m (Maybe [Role])
getRolesFromDb conn (UserId userId) =
  let q =
        [sql|
           WITH roles_for_id AS (SELECT roleid FROM user_roles WHERE userid = ?)
           SELECT label FROM roles_for_id JOIN roles ON id = roleid
        |]
   in do
        r <- liftIO $ SQLite.query conn q [userId]
        case r of
          [] -> return Nothing
          (roles :: [[DBRole]]) -> return . Just . map (\(DBRole role) -> role) $ concat roles

saveUserRoles :: SQLite.Connection -> UserId -> [Role] -> IO ()
saveUserRoles conn (UserId userid) roles =
  traverse getRoleId roles >>= \case
    [] -> throwString "no roles for given labels, shouldn't happen"
    (ids :: [Int]) ->
      forM_
        ids
        $ \roleid ->
          SQLite.execute conn "INSERT INTO user_roles (roleid, userid) VALUES (?,?)" (roleid, userid)
  where
    getRoleId :: Role -> IO Int
    getRoleId label =
      SQLite.query conn "SELECT id FROM roles WHERE label = ?" [DBRole label]
        >>= \case
          [] -> throwString "no roles for given labels, shouldn't happen"
          [SQLite.Only roleid] -> return roleid
          other -> throwString $ "unexpected DB result: " <> show other
