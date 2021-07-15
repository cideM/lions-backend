module PasswordReset.PasswordReset
  ( showResetForm,
    showChangePwForm,
    handleChangePw,
    handleReset,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.SQLite.Simple as SQLite
import qualified Error as E
import Form (FormFieldState (..))
import qualified Katip as K
import Layout (LayoutStub (..), success, warning)
import Lucid
import qualified Network.AWS.SES as SES
import Network.URI.Encode (decode, encode)
import qualified Network.Wai as Wai
import qualified PasswordReset.ChangePasswordForm as ChangePasswordForm
import qualified PasswordReset.ResetEmailForm as ResetEmailForm
import qualified PasswordReset.Token as Token
import qualified UnliftIO
import User.Types (UserId (..))
import Wai (parseParams, parseQueryParams)
import Prelude hiding (id, log)

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

-- POST handler that will try to generate a password reset token and send it by
-- email
handleReset ::
  ( MonadIO m,
    MonadReader env m,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env,
    MonadThrow m
  ) =>
  Wai.Request ->
  (Text -> Text -> IO SES.SendEmailResponse) ->
  m LayoutStub
handleReset req sendEmail' = do
  params <- liftIO $ parseParams req
  case Map.lookup "email" params of
    Nothing -> return $ formInvalid pwResetEmptyEmail
    Just email -> do
      Token.update email >>= \case
        Nothing -> return $ formInvalid pwResetEmailNotFound
        Just token -> do
          _ <- liftIO $ sendEmail' email $ encode' token
          return . passwordResetLayout $ success pwResetSuccess
  where
    encode' = T.pack . encode . T.unpack
    formInvalid = passwordResetLayout . ResetEmailForm.form . Just

showResetForm :: (MonadIO m) => m LayoutStub
showResetForm = return $ passwordResetLayout (ResetEmailForm.form Nothing)

-- The various things that I expect to happen when trying to change a user's
-- password. Each has a different error message that is shown to the user.
data TryResetError
  = InvalidPassword ChangePasswordForm.FormInput Text ChangePasswordForm.FormState
  | ParseE Token.ParseError
  deriving (Show)

-- Render the different variations of the TryResetError into Html that we can
-- send to the user directly. The goal of functions like this one is to have less noise in the handler.
renderTryResetError :: TryResetError -> LayoutStub
renderTryResetError (InvalidPassword input token state) =
  passwordChangeLayout $
    div_
      [class_ "container p-3 d-flex justify-content-center"]
      $ ChangePasswordForm.form token input state
renderTryResetError (ParseE (Token.NotFound token)) =
  passwordChangeLayout . warning . changePwTokenNotFound . T.pack $ show token
renderTryResetError (ParseE (Token.NoUser userid)) =
  passwordChangeLayout . warning . changePwUserNotFound . T.pack $ show userid
renderTryResetError (ParseE (Token.Expired expired)) =
  passwordChangeLayout . warning . changePwTokenExpired . T.pack $ show expired

makeHashedPassword ::
  ( MonadIO m,
    E.MonadError TryResetError m,
    MonadThrow m
  ) =>
  Text ->
  Text ->
  Text ->
  m Hashed
makeHashedPassword tokenInput pw pwMatch = do
  let input = ChangePasswordForm.FormInput pw pwMatch
  validPw <- E.withExceptT'' (InvalidPassword input tokenInput) $ parsePass input
  hashed <- hashPw (encodeUtf8 validPw)
  return hashed
  where
    hashPw unhashed =
      (liftIO $ BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy unhashed) >>= \case
        Nothing -> throwString "hashing password failed"
        Just pw' -> return . Hashed $ decodeUtf8 pw'

    -- I'm omitting the password length check and all that stuff. The browser will
    -- enforce a certain pattern and if someone wants to absolutely change their
    -- password with a direct POST request and they submit a one length password
    -- then so be it.
    parsePass ChangePasswordForm.FormInput {..}
      | passwordInput /= passwordInputMatch =
        Left $ ChangePasswordForm.FormState (Invalid changePwNoMatch) (Invalid changePwNoMatch)
      | otherwise =
        case ChangePasswordForm.FormState (notEmpty passwordInput) (notEmpty passwordInputMatch) of
          ChangePasswordForm.FormState (Valid _) (Valid _) -> Right passwordInput
          state -> Left state

    notEmpty "" = Invalid "Feld darf nicht leer sein"
    notEmpty v = Valid v

-- POST handler that actually changes the user's password in the database.
handleChangePw ::
  ( MonadIO m,
    MonadReader env m,
    K.KatipContext m,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env,
    MonadThrow m
  ) =>
  Wai.Request ->
  m LayoutStub
handleChangePw req = do
  params <- liftIO $ parseParams req

  case Map.lookup "token" params of
    Nothing -> return . passwordChangeLayout . warning $ changePwNoToken
    Just tok -> do
      let pw = (Map.findWithDefault "" "inputPassword" params)
          pwMatch = (Map.findWithDefault "" "inputPasswordMatch" params)

      ( E.runExceptT $
          (,)
            <$> (E.withExceptT' ParseE $ Token.parse tok)
            <*> makeHashedPassword tok pw pwMatch
        )
        >>= \case
          Left e -> do
            K.logLocM K.ErrorS $ K.ls $ show e
            return $ renderTryResetError e
          Right (Token.Valid (Token.Token {..}), hashed) -> do
            updatePw tokenUserId hashed
            return . passwordChangeLayout $ success "Password erfolgreich geändert"
  where
    updatePw userid hashed = do
      dbConn <- asks App.getDb
      UnliftIO.withRunInIO $ \runInIO ->
        SQLite.withTransaction
          dbConn
          ( runInIO $ do
              Token.delete userid
              liftIO $ updatePassword dbConn hashed userid
          )

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
