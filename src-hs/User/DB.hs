{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
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
    DBEmail (..),
  )
where

import Control.Exception.Safe
import Control.Monad (forM_)
import qualified Crypto.BCrypt as BCrypt
import Crypto.Random (SystemRandom, genBytes, newGenIO)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromField (FromField (..), ResultError (..), fieldData, returnError)
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow (..))
import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as Email
import User.Domain
  ( Role (..),
    UserEmail (..),
    UserId (..),
    UserProfile (..),
    UserProfileCreate (..),
    parseRole,
  )
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
  deriving (FromField) via Int
  deriving (ToField) via Int
  deriving (ToRow) via (SQLite.Only Int)

newtype UserProfileCreateWithPw = UserProfileCreateWithPw (Text, UserProfileCreate)

instance ToRow UserProfileCreateWithPw where
  toRow (UserProfileCreateWithPw (pw, profile)) = toField pw : toRow (DBUserProfileCreate profile)

newtype DBEmail = DBEmail EmailAddress deriving (Show)

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

newtype DBUserProfileCreate = DBUserProfileCreate UserProfileCreate deriving (Show)

instance ToRow DBUserProfileCreate where
  toRow
    ( DBUserProfileCreate
        ( UserProfileCreate
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
            _userRoles
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
            (UserId _userId)
            _userRoles
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
        toField _userBirthdayPartner,
        toField _userId
      ]

data GetUserRow = GetUserRow
  { _getUserRowRoles :: Text,
    _getUserRowUserid :: Int,
    _getUserRowEmail :: DBEmail,
    _getUserRowFirstName :: Maybe Text,
    _getUserRowLastName :: Maybe Text,
    _getUserRowAddress :: Maybe Text,
    _getUserRowMobilePhoneNr :: Maybe Text,
    _getUserRowLandlineNr :: Maybe Text,
    _getUserRowBirthday :: Maybe Time.Day,
    _getUserRowFirstNamePartner :: Maybe Text,
    _getUserRowLastNamePartner :: Maybe Text,
    _getUserRowBirthdayPartner :: Maybe Time.Day
  }
  deriving (Show)

instance FromRow GetUserRow where
  fromRow =
    GetUserRow
      <$> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field

-- TODO: Merge roles and ID into profile and then have separate version of profile for form create without ID
-- I've spent about 2h debugging this GROUP BY NULL shit, fml
-- https://stackoverflow.com/questions/3652580/how-to-prevent-group-concat-from-creating-a-result-when-no-input-data-is-present
getUser ::  SQLite.Connection -> UserId -> IO (Maybe UserProfile)
getUser conn userid = do
  rows <-
      SQLite.query
        conn
        [sql|
           SELECT
              GROUP_CONCAT(label,","),
              users.id,
              email,
              first_name,
              last_name,
              address,
              mobile_phone_nr,
              landline_nr,
              birthday,
              first_name_partner,
              last_name_partner,
              birthday_partner
           FROM users
           JOIN user_roles ON users.id = userid
           JOIN roles ON roles.id = roleid
           WHERE users.id = ?
           GROUP BY NULL
          |]
        (SQLite.Only (DBUserId userid))
  case rows of
    [] -> return Nothing
    [getUserRows :: GetUserRow] -> do
      case makeProfile getUserRows of
        Left e -> throwString $ Text.unpack ("couldn't get user: " <> e)
        Right v' -> return $ Just v'
    result -> throwString $ "got unexpected getUser result" <> show result

makeProfile :: GetUserRow -> Either Text UserProfile
makeProfile GetUserRow {_getUserRowEmail = (DBEmail email), ..} = do
  parsed <- NE.nonEmpty <$> traverse parseRole (Text.splitOn "," _getUserRowRoles)
  case parsed of
    Nothing -> Left $ "user without roles: " <> Text.pack (show _getUserRowUserid) <> " " <> Text.pack (show email)
    Just roles ->
      Right $
        UserProfile
          (UserEmail email)
          _getUserRowFirstName
          _getUserRowLastName
          _getUserRowAddress
          _getUserRowMobilePhoneNr
          _getUserRowLandlineNr
          _getUserRowBirthday
          _getUserRowFirstNamePartner
          _getUserRowLastNamePartner
          _getUserRowBirthdayPartner
          (UserId _getUserRowUserid)
          roles

deleteUserById ::  SQLite.Connection -> UserId -> IO ()
deleteUserById conn userid = do
  SQLite.execute conn "DELETE FROM users WHERE id = ?" . SQLite.Only $ DBUserId userid
  SQLite.execute conn "DELETE FROM user_roles WHERE userid = ?" . SQLite.Only $ DBUserId userid
  SQLite.execute conn "DELETE FROM event_replies WHERE userid = ?" . SQLite.Only $ DBUserId userid

getUsers ::  SQLite.Connection -> IO [UserProfile]
getUsers conn =
  handleAny
    (\e -> throwString $ "error getting users: " <> show e)
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

-- TODO: Email type
getIdAndPwByEmail ::  SQLite.Connection -> Text -> IO (Maybe (UserId, Text))
getIdAndPwByEmail conn email = do
  r <- SQLite.query conn "SELECT id, password_digest FROM users WHERE email = ?" [email]
  return $ case r of
    [(DBUserId userid, pw)] -> Just (userid, pw)
    [] -> Nothing
    _ -> throwString "unexpected result from DB for user id and password. not logging result, so please debug getIdAndPwByEmail"

updateUser :: SQLite.Connection -> UserId -> UserProfileCreate -> IO ()
updateUser conn userid profile@UserProfileCreate {..} = do
  SQLite.execute conn "DELETE FROM user_roles WHERE userid = ?" (SQLite.Only (DBUserId userid))
  saveUserRoles conn userid (NE.toList userCreateRoles)
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
    $ DBUserProfileCreate profile

saveUser :: SQLite.Connection -> UserProfileCreate -> IO ()
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
    $ UserProfileCreateWithPw (hashed, profile)

getRolesFromDb ::  SQLite.Connection -> UserId -> IO (Maybe [Role])
getRolesFromDb conn (UserId userId) =
  let q =
        [sql|
           WITH roles_for_id AS (SELECT roleid FROM user_roles WHERE userid = ?)
           SELECT label FROM roles_for_id JOIN roles ON id = roleid
        |]
   in do
        r <- SQLite.query conn q [userId]
        case r of
          [] -> return Nothing
          (roles :: [[DBRole]]) -> return . Just . map (\(DBRole role) -> role) $ concat roles

saveUserRoles :: SQLite.Connection -> UserId -> [Role] -> IO ()
saveUserRoles conn (UserId userid) roles =
  traverse getRoleId roles >>= \case
    [] -> throwString $ "no role IDs for roles: '" <> show roles <> "', shouldn't happen"
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
          [] -> throwString $ "no role for label: " <> show label
          [SQLite.Only roleid] -> return roleid
          other -> throwString $ "unexpected DB result: " <> show other
