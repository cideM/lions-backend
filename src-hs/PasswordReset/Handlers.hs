{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PasswordReset.Handlers
  ( showResetForm,
    showChangePwForm,
    handleChangePw,
    handleReset,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Lens
import qualified Crypto.BCrypt as BCrypt
import Crypto.Random (SystemRandom, genBytes, newGenIO)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Logging.Logging as Logging
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Layout (layout)
import Lucid
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Network.URI.Encode (decode, encode)
import qualified Network.Wai as Wai
import PasswordReset.DB (deleteToken, getTokenByValue, insertToken, updatePassword)
import PasswordReset.Domain (Token (..), TokenCreate (..), hashPw)
import qualified PasswordReset.Form
import Time.Time (timeDaysFromNow)
import User.DB (getIdAndPwByEmail, getUser)
import User.Domain (UserId (..))
import Wai (parseParams, parseQueryParams)

newTokenCreate :: UserId -> IO TokenCreate
newTokenCreate userid = do
  expires <- timeDaysFromNow 10
  (g :: SystemRandom) <- newGenIO
  token <- case genBytes 20 g of
    Left e -> throwString $ show e
    Right (token', _) ->
      (BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy token') >>= \case
        Nothing -> throwString "hashing reset token failed"
        Just token'' -> return $ decodeUtf8 token''
  return $ TokenCreate token expires userid

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

handleReset :: SQLite.Connection -> Int -> App.Environment -> AWS.Env -> Wai.Request -> IO (Html ())
handleReset conn port appEnv awsEnv req = do
  let resetHost =
        if appEnv == App.Production
          then "https://www.lions-achern.de"
          else Text.pack $ "http://localhost:" <> show port
  params <- parseParams req
  case Map.lookup "email" params of
    Nothing -> return $ layout "Passwort Zurücksetzen" Nothing (resetForm $ Just "Email darf nicht leer sein")
    Just email ->
      (getIdAndPwByEmail conn email) >>= \case
        Nothing -> return $ layout "Passwort Zurücksetzen" Nothing (resetForm $ Just "Diese Email-Adresse ist nicht beim Lions Club Achern registiert")
        Just (userid, _) -> do
          newToken@TokenCreate {..} <- newTokenCreate userid
          SQLite.withTransaction conn $ do
            deleteToken conn userid
            insertToken conn newToken
          let token = Text.pack . encode $ Text.unpack tokenCreateValue
              (textMail, htmlMail) = makeEmail resetHost token
              message =
                SES.message
                  (SES.content "hi")
                  (set SES.bHTML (Just $ SES.content htmlMail) (set SES.bText (Just $ SES.content textMail) SES.body))
          _ <-
            AWS.runResourceT . AWS.runAWS awsEnv . AWS.within AWS.Frankfurt . AWS.send $
              SES.sendEmail
                "hello@lions-achern.de"
                (set SES.dToAddresses ["fbeeres@gmail.com"] SES.destination)
                message
          return . layout "Passwort Zurücksetzen" Nothing $
            div_ [class_ "container p-3 d-flex justify-content-center"] $
              div_ [class_ "row col-md-6"] $ do
                p_ [class_ "alert alert-success", role_ "alert"] "Email mit Link wurde verschickt!"

resetForm :: Maybe Text -> Html ()
resetForm errMsg = do
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

showResetForm :: IO (Html ())
showResetForm = do
  return $ layout "Passwort Zurücksetzen" Nothing (resetForm Nothing)

data TryResetError
  = InvalidPassword PasswordReset.Form.FormInput Text PasswordReset.Form.FormState
  | NoTokenPassed
  | TokenNotFound Text
  | UserForTokenNotFound UserId
  | TokenExpired Token
  deriving (Show)

tryReset :: SQLite.Connection -> Map.Map Text Text -> IO (Either TryResetError ())
tryReset dbConn params = do
  case Map.lookup "token" params of
    Nothing -> return $ Left NoTokenPassed
    Just token -> do
      let input =
            PasswordReset.Form.FormInput
              (Map.findWithDefault "" "inputPassword" params)
              (Map.findWithDefault "" "inputPasswordMatch" params)
      case PasswordReset.Form.makePassword input of
        Left state -> return $ Left $ InvalidPassword input token state
        Right pw -> do
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
                      hashed <- hashPw (encodeUtf8 pw)
                      SQLite.withTransaction dbConn $ do
                        deleteToken dbConn tokenUserId
                        updatePassword dbConn hashed tokenUserId
                      return $ Right ()

handleChangePw ::
  Logging.TimedFastLogger ->
  SQLite.Connection ->
  Wai.Request ->
  IO (Html ())
handleChangePw logger conn req = do
  params <- parseParams req
  tryReset conn params >>= \case
    Left e -> do
      Logging.log logger $ show e
      case e of
        NoTokenPassed -> return $ onError noTokenMsg
        (InvalidPassword input token state) ->
          return $
            layout "Passwort Ändern" Nothing $
              div_ [class_ "container p-3 d-flex justify-content-center"] $
                PasswordReset.Form.render token input state
        (TokenNotFound token) ->
          return $ onError $ "Der Verifizierungs-Code aus der Email wurde nicht gefunden, bitte an einen Administrator wenden: " <> token
        (UserForTokenNotFound userid) ->
          return . onError . Text.pack $ "Kein Nutzer zu diesem Verifizierungs-Code registriert. Bitte an einen Administrator wenden: " <> (show userid)
        (TokenExpired tok) -> return . onError . Text.pack $ expiredMsg <> show tok
    Right _ ->
      return . layout "Passwort Zurücksetzen" Nothing $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-md-6"] $ do
            p_ [class_ "alert alert-success", role_ "alert"] "Password erfolgreich geändert"
  where
    onError :: Text -> Html ()
    onError msg =
      layout "Passwort Ändern Fehler" Nothing $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-md-6"] $ do
            p_ [class_ "alert alert-danger mb-4", role_ "alert"] $ toHtml msg

    expiredMsg = "Der Verifizierungs-Code ist bereits abgelaufen. Bitte nochmals einen neuen Link anfordern per 'Password vergessen' Knopf. Falls das Problem weiterhin besteht bitte an einen Administrator wenden: "

    noTokenMsg = "Zum Ändern des Passworts ist ein Verifizierungs-Code notwendig, der normalerweise automatisch aus dem Link in der Email herausgelesen wird. Dieser Code fehlt jedoch. Das Password kann nur über den Link in der Email geändert werden. Falls der richtige Link verwendet wurde, bitte an einen Administrator wenden."

showChangePwForm :: Wai.Request -> IO (Html ())
showChangePwForm req = do
  let params = parseQueryParams req
  case Map.lookup "token" params of
    Nothing -> throwString "password change form requires ?token= to be set, but it's empty"
    Just token -> do
      let decoded = Text.pack . decode $ Text.unpack token
      return $
        layout "Passwort Ändern" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            PasswordReset.Form.render
              decoded
              PasswordReset.Form.emptyForm
              PasswordReset.Form.emptyState
