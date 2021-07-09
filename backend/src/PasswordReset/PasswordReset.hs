module PasswordReset.PasswordReset
  ( showResetForm,
    showChangePwForm,
    handleChangePw,
    handleReset,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BCrypt
import Crypto.Random (SystemRandom, genBytes, newGenIO)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Form (FormFieldState (..))
import Layout (LayoutStub (..), success, warning)
import Lucid
import qualified Network.AWS.SES as SES
import Network.URI.Encode (decode, encode)
import qualified Network.Wai as Wai
import qualified PasswordReset.ChangePasswordForm as ChangePasswordForm
import qualified PasswordReset.ResetEmailForm as ResetEmailForm
import Time (timeDaysFromNow)
import User.DB (getCredentials, hasUser)
import User.Types (UserId (..))
import Wai (parseParams, parseQueryParams)
import Prelude hiding (id, log)

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

newtype Hashed = Hashed Text deriving (Show)

-- General layout for the password reset feature. This is NOT the same as
-- password change, which is essentially the successful outcome of the password
-- reset procedure
passwordResetLayout :: Html () -> LayoutStub
passwordResetLayout = LayoutStub "Passwort Zurücksetzen" Nothing

-- ... the continuation of the above comment. This is for actually typing in the new PW
passwordChangeLayout :: Html () -> LayoutStub
passwordChangeLayout = LayoutStub "Passwort Ändern" Nothing

updatePassword :: SQLite.Connection -> Hashed -> UserId -> IO ()
updatePassword conn hashed (UserId userid) = do
  let newPw = unhash hashed
  SQLite.execute conn "update users set password_digest = ? where id = ?" (newPw, show userid)
  where
    unhash (Hashed s) = s

getTokenByValue :: SQLite.Connection -> Text -> IO (Maybe Token)
getTokenByValue conn t =
  handleAny (\e -> throwString $ "error getting users: " <> show e) $
    (SQLite.query conn "select token, expires, id, userid from reset_tokens where token = ?" [t])
      >>= \case
        [] -> return Nothing
        [(value, expires, id, userid) :: (Text, Time.UTCTime, Int, Int)] ->
          return . Just $ Token value expires (TokenId id) (UserId userid)
        _ -> throwString . T.unpack $ "returned more than one token for value: " <> t

deleteToken :: SQLite.Connection -> UserId -> IO ()
deleteToken conn (UserId userid) = SQLite.execute conn "delete from reset_tokens where userid = ?" $ SQLite.Only userid

insertToken :: SQLite.Connection -> TokenCreate -> IO ()
insertToken conn TokenCreate {tokenCreateUserId = (UserId userid), ..} =
  SQLite.execute
    conn
    "insert into reset_tokens (token, expires, userid) values (?,?,?)"
    (tokenCreateValue, tokenCreateExpires, userid)

-- Generate a new password reset token and its expiration date
createNewToken :: IO (Text, Time.UTCTime)
createNewToken = do
  expires <- timeDaysFromNow 10
  (g :: SystemRandom) <- newGenIO
  token <- case genBytes 20 g of
    Left e -> throwString $ show e
    Right (token', _) ->
      (BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy token') >>= \case
        Nothing -> throwString "hashing reset token failed"
        Just token'' -> return $ decodeUtf8 token''
  return (token, expires)

-- Generates a new password reset token for the given user email and stores it
-- in the DB, removing whatever old tokens existed
updateUserResetToken :: SQLite.Connection -> Text -> IO (Maybe Text)
updateUserResetToken conn email =
  getCredentials conn email >>= \case
    Nothing -> return Nothing
    Just (userid, _, _) -> do
      (tokenValue, expires) <- createNewToken
      let token = TokenCreate tokenValue expires userid
      SQLite.withTransaction conn $ do
        deleteToken conn userid
        insertToken conn token
      return $ Just tokenValue

-- POST handler that will try to generate a password reset token and send it by
-- email
handleReset ::
  (MonadIO m) =>
  SQLite.Connection ->
  Wai.Request ->
  (Text -> Text -> IO SES.SendEmailResponse) ->
  m LayoutStub
handleReset conn req sendEmail' = do
  params <- liftIO $ parseParams req
  case Map.lookup "email" params of
    Nothing -> return $ formInvalid pwResetEmptyEmail
    Just email -> do
      (liftIO $ updateUserResetToken conn email) >>= \case
        Nothing -> return $ formInvalid pwResetEmailNotFound
        Just token -> do
          _ <- liftIO $ sendEmail' email $ encode' token
          return . passwordResetLayout $ success pwResetSuccess
  where
    encode' = T.pack . encode . T.unpack
    formInvalid = passwordResetLayout . ResetEmailForm.form . Just

-- GET handler that just shows a simple email input field
showResetForm ::
  (MonadIO m) =>
  m LayoutStub
showResetForm = return $ passwordResetLayout (ResetEmailForm.form Nothing)

-- I'm omitting the password length check and all that stuff. The browser will
-- enforce a certain pattern and if someone wants to absolutely change their
-- password with a direct POST request and they submit a one length password
-- then so be it.
makePassword :: ChangePasswordForm.FormInput -> Either ChangePasswordForm.FormState Text
makePassword ChangePasswordForm.FormInput {passwordInput = pw, passwordInputMatch = pw2}
  | pw /= pw2 = Left $ ChangePasswordForm.FormState (Invalid changePwNoMatch) (Invalid changePwNoMatch)
  | otherwise = case ChangePasswordForm.FormState (notEmpty pw) (notEmpty pw2) of
    ChangePasswordForm.FormState (Valid _) (Valid _) -> Right pw
    state -> Left state
  where
    notEmpty "" = Invalid "Feld darf nicht leer sein"
    notEmpty v = Valid v

-- The various things that I expect to happen when trying to change a user's
-- password. Each has a different error message that is shown to the user.
data TryResetError
  = InvalidPassword ChangePasswordForm.FormInput Text ChangePasswordForm.FormState
  | TokenNotFound Text
  | UserForTokenNotFound UserId
  | TokenExpired Token
  deriving (Show)

-- Render the different variations of the TryResetError into Html that we can
-- send to the user directly. The goal of functions like this one is to have less noise in the handler.
renderTryResetError :: TryResetError -> LayoutStub
renderTryResetError (InvalidPassword input token state) =
  passwordChangeLayout $
    div_
      [class_ "container p-3 d-flex justify-content-center"]
      $ ChangePasswordForm.form token input state
renderTryResetError (TokenNotFound token) =
  passwordChangeLayout . warning . changePwTokenNotFound . T.pack $ show token
renderTryResetError (UserForTokenNotFound userid) =
  passwordChangeLayout . warning . changePwUserNotFound . T.pack $ show userid
renderTryResetError (TokenExpired expired) =
  passwordChangeLayout . warning . changePwTokenExpired . T.pack $ show expired

-- Changes the password of a user in the database. Checks if provided passwords
-- are valid and that the token is valid.
changePasswordForToken :: SQLite.Connection -> Text -> Text -> Text -> IO (Either TryResetError ())
changePasswordForToken dbConn token pw pwMatch = do
  let input = ChangePasswordForm.FormInput pw pwMatch
  case makePassword input of
    Left state -> return $ Left $ InvalidPassword input token state
    Right validPw -> do
      getTokenByValue dbConn token >>= \case
        Nothing -> return $ Left $ TokenNotFound token
        Just tok@Token {..} -> do
          ok <- hasUser dbConn tokenUserId
          if not ok
            then (return . Left $ UserForTokenNotFound tokenUserId)
            else do
              now <- Time.getCurrentTime
              if now >= tokenExpires
                then return $ Left $ TokenExpired tok
                else do
                  hashed <- hashPw (encodeUtf8 validPw)
                  SQLite.withTransaction dbConn $ do
                    deleteToken dbConn tokenUserId
                    updatePassword dbConn hashed tokenUserId
                  return $ Right ()
  where
    hashPw unhashed =
      BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy unhashed >>= \case
        Nothing -> throwString "hashing password failed"
        Just pw' -> return . Hashed $ decodeUtf8 pw'

-- POST handler that actually changes the user's password in the database.
handleChangePw ::
  (MonadIO m) =>
  SQLite.Connection ->
  Wai.Request ->
  m LayoutStub
handleChangePw conn req = do
  params <- liftIO $ parseParams req
  case Map.lookup "token" params of
    Nothing -> return . passwordChangeLayout . warning $ changePwNoToken
    Just tok -> do
      let pw = (Map.findWithDefault "" "inputPassword" params)
          pwMatch = (Map.findWithDefault "" "inputPasswordMatch" params)
      (liftIO $ changePasswordForToken conn tok pw pwMatch) >>= \case
        Left e -> do
          -- liftIO $ log $ T.pack $ show e
          return $ renderTryResetError e
        Right _ -> return . passwordChangeLayout $ success "Password erfolgreich geändert"

-- GET handler that shows the form that lets users enter a new password.
-- Expects a token to be passed via query string parameters. That token is
-- later used to verify that the change password request is actually valid.
showChangePwForm ::
  (MonadIO m, MonadThrow m) =>
  Wai.Request ->
  m LayoutStub
showChangePwForm req = do
  case decode' <$> (Map.lookup "token" $ parseQueryParams req) of
    Nothing -> throwString "password change form requires ?token= to be set, but it's empty"
    Just token ->
      return $
        LayoutStub "Passwort Ändern" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            ChangePasswordForm.form token ChangePasswordForm.emptyForm ChangePasswordForm.emptyState
  where
    decode' = T.pack . decode . T.unpack

-- Copy that I didn't want in the handler code because it distracts from the
-- actual logic
changePwNoToken :: Text
changePwNoToken =
  [i|Zum Ändern des Passworts ist ein Verifizierungs-Code notwendig, der
  normalerweise automatisch aus dem Link in der Email herausgelesen wird.
  Dieser Code fehlt jedoch. Das Password kann nur über den Link in der Email
  geändert werden. Falls der richtige Link verwendet wurde, bitte an einen
  Administrator wenden.|]

changePwTokenExpired :: Text -> Text
changePwTokenExpired expired =
  [i|Der Verifizierungs-Code ist bereits abgelaufen. Bitte nochmals einen
  neuen Link anfordern per 'Password vergessen' Knopf. Falls das Problem
  weiterhin besteht bitte an einen Administrator wenden. Der Code ist am
  #{expired} abgelaufen.|]

changePwUserNotFound :: Text -> Text
changePwUserNotFound userid =
  [i|Kein Nutzer zu diesem Verifizierungs-Code registriert. Bitte an einen
  Administrator wenden: #{userid}|]

changePwTokenNotFound :: Text -> Text
changePwTokenNotFound token =
  [i|Der Verifizierungs-Code aus der Email wurde nicht gefunden, bitte an
  einen Administrator wenden: #{token}|]

changePwNoMatch :: Text
changePwNoMatch = "Passwörter stimmen nicht überein"

pwResetEmailNotFound :: Text
pwResetEmailNotFound = "Diese Email-Adresse ist nicht beim Lions Club Achern registiert"

pwResetEmptyEmail :: Text
pwResetEmptyEmail = "Email darf nicht leer sein"

pwResetSuccess :: Text
pwResetSuccess = "Email mit Link wurde verschickt!"
