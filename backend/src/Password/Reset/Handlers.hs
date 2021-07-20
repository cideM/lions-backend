module Password.Reset.Handlers (post, get) where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Layout (LayoutStub (..), success)
import Lucid
import qualified Network.Wai as Wai
import qualified Password.Reset.Form
import qualified Password.Reset.Mail.Types as Mail
import qualified Password.Reset.Token as Token
import qualified UnliftIO
import Wai (parseParams)
import Prelude hiding (id, log)

layout :: Html () -> LayoutStub
layout = LayoutStub "Passwort Zurücksetzen" Nothing

post ::
  ( MonadIO m,
    MonadReader env m,
    UnliftIO.MonadUnliftIO m,
    App.HasEnvironment env,
    App.HasPort env,
    App.HasMail env,
    App.HasDb env,
    MonadThrow m
  ) =>
  Wai.Request ->
  m LayoutStub
post req = do
  env <- asks App.getEnv
  port <- asks App.getPort
  sendMail <- asks App.getMail

  params <- liftIO $ parseParams req

  case Map.lookup "email" params of
    Nothing -> return $ formInvalid "Email darf nicht leer sein"
    Just emailAddr -> do
      Token.update emailAddr >>= \case
        Nothing ->
          return $
            formInvalid "Diese Email-Adresse ist nicht beim Lions Club Achern registiert"
        Just token -> do
          let resetHost =
                if env == App.Production
                  then "https://www.lions-achern.de"
                  else T.pack $ "http://localhost:" <> show port

              resetLink = resetHost <> "/passwort/aendern?token=" <> token

              email =
                Mail.Mail
                  { -- This isn't type safe at all since I could pass any kind of
                    -- text
                    mailPlainText = Mail.PlainText $ makePlainEmail resetLink,
                    mailHtml = Mail.Html $ makeHtmlEmail resetLink,
                    mailTitle = "Passwort Zurücksetzen"
                  }

          _ <- liftIO $ sendMail emailAddr email

          return . layout $ success "Email mit Link wurde verschickt!"
  where
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
