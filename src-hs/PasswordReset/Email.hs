{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PasswordReset.Email where

import Data.String.Interpolate (i)
import Data.Text (Text)
import Control.Lens
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES

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
