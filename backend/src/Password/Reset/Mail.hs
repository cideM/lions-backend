module Password.Reset.Mail
  ( sendAws,
    Mail (..),
    SendMail,
    sendIoRef,
    PlainText (..),
    Html (..),
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.SES as SES
import Amazonka.SES.Types.Body (body_html, body_text)
import Amazonka.SES.Types.Destination (destination_toAddresses)
import Control.Exception.Safe
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Text (Text)

newtype PlainText = PlainText Text

newtype Html = Html Text

data Mail = Mail
  { mailPlainText :: PlainText,
    mailHtml :: Html,
    mailTitle :: Text
  }

type SendMail m = Text -> Mail -> m SES.SendEmailResponse

-- Function for sending email through AWS. This is exported because it's
-- partially applied in Main and then passed to the handler, so the handler can
-- be tested without sending emails.
sendAws ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  AWS.Env ->
  SendMail m
sendAws awsEnv recipient Mail {..} =
  let (Html htmlContent) = mailHtml
      (PlainText plainContent) = mailPlainText
      bodyWithHTML = set body_html (Just $ SES.newContent htmlContent) SES.newBody
      bodyWithTextAndHTML = set body_text (Just $ SES.newContent plainContent) bodyWithHTML
      message =
        SES.newMessage
          (SES.newContent mailTitle)
          (bodyWithTextAndHTML)
   in liftIO
        . AWS.runResourceT
        . AWS.send awsEnv
        $ SES.newSendEmail
          "hello@lions-achern.de"
          (set destination_toAddresses (Just [recipient]) SES.newDestination)
          message

-- This is a version of SendMail that records sent mails in an IO ref. Use it
-- in unit or integration tests, to avoid using the actual AWS infrastructure.
sendIoRef :: IORef (Maybe Text, Maybe Mail) -> SendMail IO
sendIoRef ref recipient mail = do
  IORef.writeIORef ref (Just recipient, Just mail)
  return $ SES.newSendEmailResponse 0 ""
