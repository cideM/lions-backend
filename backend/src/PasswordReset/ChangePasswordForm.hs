module PasswordReset.ChangePasswordForm
  ( form,
    emptyForm,
    emptyState,
    FormInput (..),
    FormState (..),
  )
where

import Data.Text (Text)
import Form (FormFieldState (..), processField)
import Layout (describedBy_)
import Lucid

-- Form state boilerplate for submitNewPasswordForm
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

-- The form where a user can enter a new password and also confirm it
form :: Text -> FormInput -> FormState -> Html ()
form token FormInput {..} FormState {..} = do
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
