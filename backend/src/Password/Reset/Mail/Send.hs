module Password.Reset.Mail.Send (send) where

import Control.Exception.Safe
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Password.Reset.Mail.Types

-- Function for sending email through AWS. This is exported because it's
-- partially applied in Main and then passed to the handler, so the handler can
-- be tested without sending emails.
send ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  AWS.Env ->
  Text ->
  Mail ->
  m SES.SendEmailResponse
send awsEnv recipient Mail {..} =
  let (Html htmlContent) = mailHtml
      (PlainText plainContent) = mailPlainText
      message =
        SES.message
          (SES.content mailTitle)
          ( set
              SES.bHTML
              (Just $ SES.content htmlContent)
              (set SES.bText (Just $ SES.content plainContent) SES.body)
          )
   in liftIO
        . AWS.runResourceT
        . AWS.runAWS awsEnv
        . AWS.within AWS.Frankfurt
        . AWS.send
        $ SES.sendEmail
          "hello@lions-achern.de"
          (set SES.dToAddresses [recipient] SES.destination)
          message
