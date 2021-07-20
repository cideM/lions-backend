module Password.Reset.Mail.Types
  ( Mail (..),
    SendMail,
    PlainText (..),
    Html (..),
  )
where

import Data.Text (Text)
import qualified Network.AWS.SES as SES

newtype PlainText = PlainText Text

newtype Html = Html Text

data Mail = Mail
  { mailPlainText :: PlainText,
    mailHtml :: Html,
    mailTitle :: Text
  }

type SendMail m = Text -> Mail -> m SES.SendEmailResponse
