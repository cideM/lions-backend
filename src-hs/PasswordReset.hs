{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PasswordReset
  ( showResetForm,
    showChangePwForm,
    sendMail,
    handleChangePw,
    handleReset,
  )
where

import Control.Exception.Safe
import Control.Lens
import qualified Crypto.BCrypt as BCrypt
import Crypto.Random (SystemRandom, genBytes, newGenIO)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow (FromRow)
import Form.Form (FormFieldState (..), processField)
import GHC.Generics
import Layout (describedBy_, layout, success, warning)
import qualified Logging.Logging as Logging
import Lucid
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Network.URI.Encode (decode, encode)
import qualified Network.Wai as Wai
import Time.Time (timeDaysFromNow)
import User.DB (getIdAndPwByEmail, getUser)
import User.Domain (UserId (..))
import Wai (parseParams, parseQueryParams)
import Prelude hiding (id)

newtype TokenId = TokenId Int
  deriving (Show, Eq)
  deriving (Generic)

instance ToJSON TokenId where
  toEncoding = genericToEncoding defaultOptions

data TokenCreate = TokenCreate
  { tokenCreateValue :: Text,
    tokenCreateExpires :: Time.UTCTime,
    tokenCreateUserId :: UserId
  }
  deriving (Show, Eq, Generic)

data Token = Token
  { tokenValue :: Text,
    tokenExpires :: Time.UTCTime,
    tokenId :: TokenId,
    tokenUserId :: UserId
  }
  deriving (Show, Eq, Generic)

instance ToJSON Token where
  toEncoding = genericToEncoding defaultOptions

newtype Hashed = Hashed Text deriving (Show)

hashPw :: ByteString -> IO Hashed
hashPw unhashed =
  BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy unhashed >>= \case
    Nothing -> throwString "hashing password failed"
    Just pw' -> return . Hashed $ decodeUtf8 pw'

unhash :: Hashed -> Text
unhash (Hashed s) = s

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
  getIdAndPwByEmail conn email >>= \case
    Nothing -> return Nothing
    Just (userid, _) -> do
      (tokenValue, expires) <- createNewToken
      let token = TokenCreate tokenValue expires userid
      SQLite.withTransaction conn $ do
        deleteToken conn userid
        insertToken conn token
      return $ Just tokenValue

-- Form that is shown when users want to reset their passwords
emailInput :: Maybe Text -> Html ()
emailInput errMsg = do
  div_ [class_ "container p-2"] $ do
    div_ [class_ "row d-flex justify-content-center"] $ do
      div_ [class_ "col-md-6"] $ do
        h1_ [class_ "h4 mb-3"] "Passwort Zurücksetzen"
        case errMsg of
          Nothing -> mempty
          Just msg -> p_ [class_ "my-3 alert alert-danger"] $ toHtml msg
        p_ [class_ "my-3 alert alert-secondary"] "Bitte die Email-Adresse eintragen, mit der du beim Lions Club Achern angemeldet bist. Es wird dann ein Link an diese Email-Adresse verschickt, über welchen du dein Passwort ändern kannst."
        form_ [action_ "/passwort/link", method_ "post"] $ do
          label_ [for_ "emailInput", class_ "form-label"] "Email Adresse"
          input_ [required_ "required", class_ "form-control", name_ "email", id_ "emailInput", placeholder_ "platzhalter@email.de"]
          button_ [class_ "mt-3 btn btn-primary", type_ "submit"] "Absenden"

data FormInput = FormInput
  { passwordInput :: Text,
    passwordInputMatch :: Text
  }
  deriving (Show)

emptyForm :: FormInput
emptyForm = FormInput "" ""

data FormState = FormState
  { passwordState :: FormFieldState Text,
    passwordStateMatch :: FormFieldState Text
  }
  deriving (Show)

emptyState :: FormState
emptyState = FormState NotValidated NotValidated

-- I'm omitting the password length check and all that stuff. The browser will
-- enforce a certain pattern and if someone wants to absolutely change their
-- password with a direct POST request and they submit a one length password
-- then so be it.
makePassword :: FormInput -> Either FormState Text
makePassword FormInput {..}
  | passwordInput /= passwordInputMatch = Left $ FormState (Invalid "Passwörter stimmen nicht überein") (Invalid "Passwörter stimmen nicht überein")
  | otherwise = case FormState (notEmpty passwordInput) (notEmpty passwordInputMatch) of
    FormState (Valid _) (Valid pw) -> Right pw
    state -> Left state
  where
    notEmpty "" = Invalid "Feld darf nicht leer sein"
    notEmpty v = Valid v

-- The form where a user can enter a new password and also confirm it
submitNewPasswordForm :: Text -> FormInput -> FormState -> Html ()
submitNewPasswordForm token FormInput {..} FormState {..} = do
  form_ [method_ "post", action_ "/passwort/aendern"] $ do
    div_ [class_ "row row-cols-1 g-2"] $ do
      div_ $ do
        let (className, errMsg) = processField passwordState
        label_ [class_ "form-label", for_ "inputPassword"] "Passwort"
        input_
          [ class_ className,
            type_ "password",
            id_ "inputPassword",
            required_ "required",
            describedBy_ "validationPwFeedback inputPasswordHelp",
            value_ passwordInput,
            name_ "inputPassword",
            title_ "Passwort muss zwischen 6 und 30 Zeichen lang sein",
            pattern_ ".{6,30}"
          ]
        div_ [id_ "inputPasswordHelp", class_ "form-text"] "Passwort muss zwischen 6 und 30 Zeichen lang sein"
        maybe mempty (div_ [id_ "validationPwFeedback", class_ "invalid-feedback"] . toHtml) errMsg
      div_ $ do
        let (className, errMsg) = processField passwordStateMatch
        label_ [class_ "form-label", for_ "inputPasswordMatch"] "Passwort wiederholen"
        input_
          [ class_ className,
            type_ "password",
            id_ "inputPasswordMatch",
            required_ "required",
            describedBy_ "validationPwMatchFeedback inputPasswordMatchHelp",
            value_ passwordInputMatch,
            name_ "inputPasswordMatch",
            title_ "Passwort muss zwischen 6 und 30 Zeichen lang sein",
            pattern_ ".{6,30}"
          ]
        div_ [id_ "inputPasswordMatchHelp", class_ "form-text"] "Passwort muss zwischen 6 und 30 Zeichen lang sein"
        maybe mempty (div_ [id_ "validationPwMatchFeedback", class_ "invalid-feedback"] . toHtml) errMsg
        input_ [type_ "hidden", value_ token, name_ "token", id_ "token"]
      div_ $ do
        button_ [type_ "submit", class_ "btn btn-primary"] "Speichern"

newtype DBToken = DBToken Token deriving (Show)

instance FromRow DBToken where
  fromRow =
    DBToken
      <$> ( Token <$> SQLite.field
              <*> SQLite.field
              <*> (TokenId <$> SQLite.field)
              <*> (UserId <$> SQLite.field)
          )

updatePassword :: SQLite.Connection -> Hashed -> UserId -> IO ()
updatePassword conn hashed (UserId userid) = do
  let newPw = unhash hashed
  SQLite.execute conn "update users set password_digest = ? where id = ?" (newPw, show userid)

getTokenByValue :: SQLite.Connection -> Text -> IO (Maybe Token)
getTokenByValue conn t =
  handleAny (\e -> throwString $ "error getting users: " <> show e) $
    (SQLite.query conn "select token, expires, id, userid from reset_tokens where token = ?" [t])
      >>= \case
        [] -> return Nothing
        [(DBToken token) :: DBToken] -> return $ Just token
        _ -> throwString . Text.unpack $ "returned more than one token for value: " <> t

deleteToken :: SQLite.Connection -> UserId -> IO ()
deleteToken conn (UserId userid) = SQLite.execute conn "delete from reset_tokens where userid = ?" $ SQLite.Only userid

insertToken :: SQLite.Connection -> TokenCreate -> IO ()
insertToken conn TokenCreate {tokenCreateUserId = (UserId userid), ..} =
  SQLite.execute
    conn
    "insert into reset_tokens (token, expires, userid) values (?,?,?)"
    (tokenCreateValue, tokenCreateExpires, userid)

-- Create a password reset email, both as a plain text and an HTML mail
makeEmail :: Text -> Text -> (Text, Text)
makeEmail host token =
  let resetLink = host <> "/passwort/aendern?token=" <> token
      textEmail =
        [i|
          Hallo liebes Lions Mitglied,

          zum Ändern des Passworts einfach den folgenden Link anklicken:
          #{resetLink}

          Viele Grüße,
          Dein Lions Club Achern
        |]
      htmlMail =
        [i|
          <h3>Hallo liebes Lions Mitglied,</h3>
          <p>
            zum Ändern des Passworts einfach den folgenden Link anklicken:<br>
            <a href="#{resetLink}">Link</a>
          </p>

          <p>
            Viele Grüße,<br>
            Dein Lions Club Achern
          </p>
        |]
   in (textEmail, htmlMail)

sendMail :: AWS.Env -> Text -> Text -> Text -> IO SES.SendEmailResponse
sendMail awsEnv resetHost email token =
  let (textMail, htmlMail) = makeEmail resetHost token
      message =
        SES.message
          (SES.content "Passwort Zurücksetzen")
          (set SES.bHTML (Just $ SES.content htmlMail) (set SES.bText (Just $ SES.content textMail) SES.body))
   in AWS.runResourceT . AWS.runAWS awsEnv . AWS.within AWS.Frankfurt . AWS.send $
        SES.sendEmail
          "hello@lions-achern.de"
          (set SES.dToAddresses [email] SES.destination)
          message

-- General layout for the password reset feature. This is NOT the same as
-- password change, which is essentially the successful outcome of the password
-- reset procedure
passwordResetLayout = layout "Passwort Zurücksetzen" Nothing

-- ... the continuation of the above comment. This is for actually typing in the new PW
passwordChangeLayout = layout "Passwort Ändern" Nothing

-- POST handler that will try to generate a password reset token and send it by
-- email
handleReset ::
  SQLite.Connection ->
  Wai.Request ->
  (Text -> Text -> IO SES.SendEmailResponse) ->
  IO (Html ())
handleReset conn req sendEmail' = do
  Map.lookup "email" <$> parseParams req >>= \case
    Nothing -> return noParamHtml
    Just email -> do
      updateUserResetToken conn email >>= \case
        Nothing -> return emailNotFoundHtml
        Just token -> (sendEmail' email . Text.pack . encode $ Text.unpack token) >> return successHtml
  where
    emailNotFoundHtml = passwordResetLayout (emailInput $ Just "Diese Email-Adresse ist nicht beim Lions Club Achern registiert")
    noParamHtml = passwordResetLayout (emailInput $ Just "Email darf nicht leer sein")
    successHtml = passwordResetLayout $ success "Email mit Link wurde verschickt!"

-- GET handler that just shows a simple email input field
showResetForm :: IO (Html ())
showResetForm = return $ passwordResetLayout (emailInput Nothing)

data TryResetError
  = InvalidPassword FormInput Text FormState
  | TokenNotFound Text
  | UserForTokenNotFound UserId
  | TokenExpired Token
  deriving (Show)

-- Changes the password of a user in the database. Checks if provided passwords
-- are valid and that the token is valid.
changePasswordForToken ::
  SQLite.Connection ->
  Text ->
  Text ->
  Text ->
  IO (Either TryResetError ())
changePasswordForToken dbConn token pw pwMatch = do
  let input = FormInput pw pwMatch
  case makePassword input of
    Left state -> return $ Left $ InvalidPassword input token state
    Right validPw -> do
      getTokenByValue dbConn token >>= \case
        Nothing -> return $ Left $ TokenNotFound token
        Just tok@Token {..} -> do
          getUser dbConn tokenUserId >>= \case
            Nothing -> return $ Left $ UserForTokenNotFound tokenUserId
            Just _ -> do
              now <- Time.getCurrentTime
              if now >= tokenExpires
                then return $ Left $ TokenExpired tok
                else do
                  hashed <- hashPw (encodeUtf8 validPw)
                  SQLite.withTransaction dbConn $ do
                    deleteToken dbConn tokenUserId
                    updatePassword dbConn hashed tokenUserId
                  return $ Right ()

-- POST handler that actually changes the user's password in the database.
handleChangePw ::
  Logging.TimedFastLogger ->
  SQLite.Connection ->
  Wai.Request ->
  IO (Html ())
handleChangePw logger conn req = do
  params <- parseParams req
  case Map.lookup "token" params of
    Nothing -> return $ onError noTokenMsg
    Just tok -> do
      let pw = (Map.findWithDefault "" "inputPassword" params)
          pwMatch = (Map.findWithDefault "" "inputPasswordMatch" params)
      changePasswordForToken conn tok pw pwMatch >>= \case
        Left e -> do
          Logging.log logger $ show e
          case e of
            (InvalidPassword input token state) -> return $ onFormInvalid input token state
            (TokenNotFound token) -> return $ onTokenNotFound token
            (UserForTokenNotFound userid) -> return $ onUserForTokenNotFound userid
            (TokenExpired expired) -> return $ onExpired expired
        Right _ -> return onSuccess
  where
    onExpired tok = onError . Text.pack $ expiredMsg <> show tok
    onFormInvalid input token state =
      passwordChangeLayout $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          submitNewPasswordForm token input state
    onUserForTokenNotFound userid = onError . Text.pack $ "Kein Nutzer zu diesem Verifizierungs-Code registriert. Bitte an einen Administrator wenden: " <> (show userid)
    onTokenNotFound token = onError $ "Der Verifizierungs-Code aus der Email wurde nicht gefunden, bitte an einen Administrator wenden: " <> token
    onSuccess = passwordChangeLayout $ success "Password erfolgreich geändert"
    onError = passwordChangeLayout . warning
    expiredMsg = "Der Verifizierungs-Code ist bereits abgelaufen. Bitte nochmals einen neuen Link anfordern per 'Password vergessen' Knopf. Falls das Problem weiterhin besteht bitte an einen Administrator wenden: "
    noTokenMsg = "Zum Ändern des Passworts ist ein Verifizierungs-Code notwendig, der normalerweise automatisch aus dem Link in der Email herausgelesen wird. Dieser Code fehlt jedoch. Das Password kann nur über den Link in der Email geändert werden. Falls der richtige Link verwendet wurde, bitte an einen Administrator wenden."

-- GET handler that shows the form that lets users enter a new password.
showChangePwForm :: Wai.Request -> IO (Html ())
showChangePwForm req = do
  case decode' <$> (Map.lookup "token" $ parseQueryParams req) of
    Nothing -> throwString "password change form requires ?token= to be set, but it's empty"
    Just token ->
      return $
        layout "Passwort Ändern" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            submitNewPasswordForm token emptyForm emptyState
  where
    decode' = Text.pack . decode . Text.unpack
