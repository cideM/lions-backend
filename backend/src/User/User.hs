module User.User
  ( exists,
    get,
    getCredentials,
    getAll,
    save,
    update,
    delete,
    Profile (..),
  )
where

import qualified App
import Control.Error hiding (tryIO, tryJust)
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Crypto.KDF.BCrypt (hashPassword)
import qualified Crypto.Random
import Data.Aeson
  ( ToJSON,
    defaultOptions,
    genericToEncoding,
    toEncoding,
  )
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow (..))
import GHC.Generics
import Text.Email.Validate (emailAddress)
import qualified Text.Email.Validate as Email
import User.Email (Email (..))
import qualified User.Id as User
import qualified User.Role.DB as Role
import qualified User.Role.Role as Role
import Prelude hiding (id)

data Profile = Profile
  { userEmail :: Email,
    userFirstName :: Maybe Text,
    userLastName :: Maybe Text,
    userAddress :: Maybe Text,
    userMobilePhoneNr :: Maybe Text,
    userLandlineNr :: Maybe Text,
    userBirthday :: Maybe Text,
    userFirstNamePartner :: Maybe Text,
    userLastNamePartner :: Maybe Text,
    userBirthdayPartner :: Maybe Text,
    userRoles :: NonEmpty Role.Role
  }
  deriving (Show, Generic)

instance ToJSON Profile where
  toEncoding = genericToEncoding defaultOptions

newtype ProfileCreateWithPw = ProfileCreateWithPw (Text, Profile)

instance ToRow ProfileCreateWithPw where
  toRow (ProfileCreateWithPw (pw, profile)) = toField pw : toRow profile

instance ToRow Profile where
  toRow
    ( Profile
        (Email _userEmail)
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
      ) =
      [ toField (decodeUtf8 $ Email.toByteString _userEmail),
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

exists ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  User.Id ->
  m Bool
exists (User.Id id) = do
  conn <- asks App.getDb
  rows <-
    liftIO $ SQLite.query conn [sql| SELECT id FROM users WHERE users.id = ?|] [id]
  case rows of
    [] -> return False
    [[_ :: Integer]] -> return True
    result -> throwString $ "got unexpected user exists result" <> show result

-- I've spent about 2h debugging this GROUP BY NULL shit, fml
-- https://stackoverflow.com/questions/3652580/how-to-prevent-group-concat-from-creating-a-result-when-no-input-data-is-present
get ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  User.Id ->
  m (Maybe (User.Id, Profile))
get (User.Id userid) = do
  conn <- asks App.getDb
  rows <-
    liftIO $
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
        [userid]
  case rows of
    [] -> return Nothing
    [getUserRows :: GetUserRow] -> do
      case parseUserRow getUserRows of
        Left e -> throwString $ Text.unpack ("couldn't get user: " <> e)
        Right v' -> return $ Just v'
    result -> throwString $ "got unexpected getUser result" <> show result

parseUserRow :: GetUserRow -> Either Text (User.Id, Profile)
parseUserRow GetUserRow {..} = do
  parsed <- NE.nonEmpty <$> traverse Role.parse (Text.splitOn "," _getUserRowRoles)
  roles <- note [i|user without roles; id: #{_getUserRowUserid} email: ${_getUserRowEmail}|] parsed
  parsedEmail <- note [i|couldn't parse email #{_getUserRowEmail}|] $ emailAddress (encodeUtf8 _getUserRowEmail)
  Right $
    ( User.Id _getUserRowUserid,
      Profile
        (Email parsedEmail)
        _getUserRowFirstName
        _getUserRowLastName
        _getUserRowAddress
        _getUserRowMobilePhoneNr
        _getUserRowLandlineNr
        _getUserRowBirthday
        _getUserRowFirstNamePartner
        _getUserRowLastNamePartner
        _getUserRowBirthdayPartner
        roles
    )

data GetUserRow = GetUserRow
  { _getUserRowRoles :: Text,
    _getUserRowUserid :: Int,
    _getUserRowEmail :: Text,
    _getUserRowFirstName :: Maybe Text,
    _getUserRowLastName :: Maybe Text,
    _getUserRowAddress :: Maybe Text,
    _getUserRowMobilePhoneNr :: Maybe Text,
    _getUserRowLandlineNr :: Maybe Text,
    _getUserRowBirthday :: Maybe Text,
    _getUserRowFirstNamePartner :: Maybe Text,
    _getUserRowLastNamePartner :: Maybe Text,
    _getUserRowBirthdayPartner :: Maybe Text
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

newtype UserProfileWithId = UserProfileWithId (User.Id, Profile)

instance ToRow UserProfileWithId where
  toRow (UserProfileWithId (User.Id uid, Profile {..})) =
    let (Email email) = userEmail
     in [ toField (decodeUtf8 $ Email.toByteString email),
          toField userFirstName,
          toField userLastName,
          toField userAddress,
          toField userMobilePhoneNr,
          toField userLandlineNr,
          toField userBirthday,
          toField userFirstNamePartner,
          toField userLastNamePartner,
          toField userBirthdayPartner,
          toField uid
        ]

delete ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  User.Id ->
  m ()
delete (User.Id userid) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "DELETE FROM users WHERE id = ?" [userid]
  liftIO $ SQLite.execute conn "DELETE FROM user_roles WHERE userid = ?" [userid]
  liftIO $ SQLite.execute conn "DELETE FROM event_replies WHERE userid = ?" [userid]

getAll ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  m [(User.Id, Profile)]
getAll = do
  conn <- asks App.getDb
  rows <-
    liftIO $
      SQLite.query_
        conn
        [sql|
         SELECT GROUP_CONCAT(label,","), users.id, email, first_name, last_name, address, mobile_phone_nr, landline_nr,
                birthday, first_name_partner, last_name_partner, birthday_partner
         FROM users
         JOIN user_roles ON users.id = userid
         JOIN roles ON roles.id = roleid
         GROUP BY users.id
        |]

  case traverse parseUserRow rows of
    Left e -> throwString $ "error making profile: " <> Text.unpack e
    Right v -> pure v

update ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  User.Id ->
  Profile ->
  m ()
update uid@(User.Id userid) profile@Profile {..} = do
  conn <- asks App.getDb

  -- We'll delete all user roles and redo them
  liftIO $ SQLite.execute conn "DELETE FROM user_roles WHERE userid = ?" [userid]
  Role.save uid (NE.toList userRoles)

  liftIO $
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
      (UserProfileWithId (uid, profile))

save ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Profile ->
  m ()
save profile = do
  conn <- asks App.getDb
  (password :: ByteString) <- liftIO $ do Crypto.Random.getRandomBytes 20
  (hashed :: ByteString) <- liftIO $ hashPassword 12 password

  liftIO
    $ SQLite.execute
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
    $ ProfileCreateWithPw ((decodeUtf8 hashed), profile)

-- Returns ID, hashed password and salt used for hashing. This will be a
-- non-empty text for firebase data that was imported. It will be empty for all
-- other users.
getCredentials ::
  ( MonadIO m,
    Monad m,
    MonadPlus m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Text ->
  m (User.Id, Maybe Text, Text)
getCredentials email = do
  conn <- asks App.getDb
  let withoutSpace = Text.strip email
  r <- liftIO $ SQLite.query conn "select id, salt, password_digest from users where email = ? collate nocase" [withoutSpace]
  case r of
    [(userid, salt, pw)] ->
      case salt of
        Nothing -> pure ((User.Id userid), Nothing, pw)
        Just "" -> pure ((User.Id userid), Nothing, pw)
        Just salt' -> pure ((User.Id userid), Just salt', pw)
    [] -> mzero
    _ -> throwString "unexpected result from DB for user id and password. not logging result, so please debug getCredentials"
