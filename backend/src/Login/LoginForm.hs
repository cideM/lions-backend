module Login.LoginForm (render, FormState (..), FormInput (..), emptyForm) where

import Data.Text (Text)
import Form (FormFieldState (..), processField)
import Layout (ActiveNavLink (..), LayoutStub (..), describedBy_)
import Lucid
import User.Session (Authentication, isAuthenticated)
import Prelude hiding (id)

{--
   Normally the form state would be used in a parsing/validation function. But
   in the case of the login form I just check if credentials match, even if
   both password and user are empty. Therefore the state in this file isn't
   really used much.
--}
data FormState = FormState
  { loginStateEmail :: FormFieldState Text,
    loginStatePassword :: FormFieldState Text
  }
  deriving (Show)

emptyForm :: FormState
emptyForm =
  FormState
    { loginStateEmail = NotValidated,
      loginStatePassword = NotValidated
    }

data FormInput = FormInput
  { inputEmail :: Text,
    inputPassword :: Text
  }
  deriving (Show)

-- The actual login form. You didn't think it'd be this much code, did you?
render :: Authentication -> FormInput -> FormState -> LayoutStub
render auth FormInput {..} FormState {..} =
  if isAuthenticated auth
    then LayoutStub "Login" (Just Login) $ do
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Du bist bereits eingelogged!"
          form_ [class_ "p-0", method_ "post", action_ "/logout"] $ do
            button_ [class_ "btn btn-primary", type_ "submit", autofocus_] "Ausloggen"
    else do
      LayoutStub "Login" (Just Login) $
        div_ [class_ "container-md d-flex justify-content-center p-3"] $ do
          form_ [class_ "col-8", method_ "post", action_ "/login"] $ do
            div_ [class_ "row row-cols-8 g-2"] $ do
              div_ [class_ "mb-3 form-floating"] $ do
                let (className, errMsg) = processField loginStateEmail
                input_
                  [ class_ className,
                    type_ "email",
                    name_ "email",
                    id_ "email",
                    required_ "required",
                    value_ inputEmail,
                    autofocus_,
                    describedBy_ "invalidEmailFeedback",
                    placeholder_ "hallo@gmail.com"
                  ]
                label_ [class_ "form-label", for_ "email"] "Email Adresse"
                maybe mempty (div_ [id_ "invalidEmailFeedback", class_ "invalid-feedback"] . toHtml) errMsg
            div_ [class_ "row row-cols-8 g-2"] $ do
              div_ [class_ "mb-3 form-floating"] $ do
                let (className, errMsg) = processField loginStatePassword
                input_
                  [ class_ className,
                    type_ "password",
                    name_ "password",
                    id_ "password",
                    required_ "required",
                    value_ inputPassword,
                    describedBy_ "invalidPasswordFeedback",
                    placeholder_ "foobar"
                  ]
                label_ [class_ "form-label", for_ "password"] "Passwort"
                maybe mempty (div_ [id_ "invalidPasswordFeedback", class_ "invalid-feedback"] . toHtml) errMsg
            div_ [class_ "row g-2"] $ do
              div_ [class_ "col-md-6"] $ do
                button_ [class_ "w-100 btn btn-primary", type_ "submit"] "Einloggen"
              div_ [class_ "col-md-6"] $ do
                a_ [class_ "w-100 btn btn-secondary", href_ "/passwort/link", role_ "button"] "Passwort vergessen"
