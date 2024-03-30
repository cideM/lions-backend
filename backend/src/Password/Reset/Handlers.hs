module Password.Reset.Handlers (post, get) where

import qualified Amazonka as AWS
import qualified Amazonka.SES as SES
import qualified Amazonka.SES.Types as SES
import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Katip as K
import Layout (LayoutStub (..), success)
import Lucid
import qualified Network.Wai as Wai
import qualified Password.Reset.Form
import qualified Password.Reset.Token as Token
import qualified UnliftIO
import Wai (parseParams)
import Prelude hiding (id, log)

layout :: Html () -> LayoutStub
layout = LayoutStub "Passwort Zurücksetzen"

post ::
  ( MonadIO m,
    MonadReader env m,
    UnliftIO.MonadUnliftIO m,
    K.KatipContext m,
    App.HasAWS env,
    App.HasDb env,
    MonadThrow m
  ) =>
  Wai.Request ->
  m LayoutStub
post req = do
  awsEnv <- asks App.getAWSEnv

  K.katipAddNamespace "password_reset_post" $ do
    params <- liftIO $ parseParams req

    case Map.lookup "email" params of
      Nothing -> do
        K.logLocM K.InfoS "no email param found"
        return $ formInvalid "Email darf nicht leer sein"
      Just emailAddr -> do
        K.katipAddContext (K.sl "email" ([i|'#{emailAddr}'|] :: Text)) $ do
          Token.update emailAddr >>= \case
            Nothing -> do
              K.logLocM K.InfoS "email not found"
              return $
                formInvalid "Diese Email-Adresse ist nicht beim Lions Club Achern registiert"
            Just token -> do
              K.logLocM K.InfoS "email found"

              -- TODO: Extract o a more prominent place
              let resetHost = "https://mitglieder.lions-achern.de"
                  resetLink = resetHost <> "/passwort/aendern?token=" <> token

              _ <-
                liftIO $
                  sendMail
                    awsEnv
                    emailAddr
                    (EmailHtml $ makeHtmlEmail resetLink)
                    (EmailPlainText $ makePlainEmail resetLink)
                    "Passwort Zurücksetzen"

              return . layout $ success "Email mit Link wurde verschickt!"
  where
    sendMail ::
      (MonadIO m, MonadThrow m) =>
      AWS.Env ->
      Text ->
      EmailHtml ->
      EmailPlainText ->
      Text ->
      m SES.SendEmailResponse
    sendMail
      awsEnv
      recipient
      (EmailHtml htmlContent)
      (EmailPlainText plainContent)
      title = do
        let envFrankfurt = awsEnv {AWS.region = AWS.Frankfurt}

        let message =
              SES.newMessage
                (SES.newContent title)
                ( SES.newBody
                    { SES.html = Just $ SES.newContent htmlContent,
                      SES.text = Just $ SES.newContent plainContent
                    }
                )

        let destination = SES.newDestination {SES.toAddresses = Just [recipient]}

        liftIO . AWS.runResourceT . AWS.send envFrankfurt $
          SES.newSendEmail "hello@lions-achern.de" destination message

    formInvalid = layout . Password.Reset.Form.render . Just

    makeHtmlEmail resetLink =
      ( [i|
              <h3>Hallo liebes Lions Mitglied,</h3>
              <p>
                zum Ändern des Passworts einfach den folgenden Link anklicken:<br>
                <a href="#{resetLink}">Link</a>
              </p>

              <p>
                Viele Grüße,<br>
                Dein Lions Club Achern
              </p>
            |] ::
          Text
      )

    makePlainEmail resetLink =
      ( [i|
              Hallo liebes Lions Mitglied,

              zum Ändern des Passworts einfach den folgenden Link anklicken:
              #{resetLink}

              Viele Grüße,
              Dein Lions Club Achern
            |] ::
          Text
      )

get :: (MonadIO m) => m LayoutStub
get = return $ layout (Password.Reset.Form.render Nothing)

-- TODO: Move
newtype EmailPlainText = EmailPlainText Text

newtype EmailHtml = EmailHtml Text
