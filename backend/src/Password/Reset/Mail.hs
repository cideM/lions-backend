module Password.Reset.Mail
  ( sendAws,
    Mail (..),
    SendMail,
    sendIoRef,
    PlainText (..),
    Html (..),
  )
where

import Control.Exception.Safe
import Lens.Micro (set)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES

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

-- This is a version of SendMail that records sent mails in an IO ref. Use it
-- in unit or integration tests, to avoid using the actual AWS infrastructure.
sendIoRef :: IORef (Maybe Text, Maybe Mail) -> SendMail IO
sendIoRef ref recipient mail = do
  IORef.writeIORef ref (Just recipient, Just mail)
  return $ SES.sendEmailResponse 0 "id"
