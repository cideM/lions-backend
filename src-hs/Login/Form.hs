{-# LANGUAGE OverloadedStrings #-}

module Login.Form (render, LoginFormState (..)) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Layout (ActiveNavLink (..), describedBy_, layout)
import Lucid
import Prelude hiding (id)

type Email = Text

type EmailError = Text

type Pw = Text

type PwError = Text

data LoginFormState = NotLoggedInNotValidated | NotLoggedInValidated Email (Maybe EmailError) Pw (Maybe PwError) | LoggedIn

render :: LoginFormState -> Html ()
render LoggedIn =
  layout "Login" (Just Login) $ do
    div_ [class_ "container p-3 d-flex justify-content-center"] $
      div_ [class_ "row col-6"] $ do
        p_ [class_ "alert alert-secondary", role_ "alert"] "Du bist bereits eingelogged!"
        form_ [class_ "p-0", method_ "post", action_ "/logout"] $ do
          button_ [class_ "btn btn-primary", type_ "submit", autofocus_] "Ausloggen"
render formState =
  let (email, emailClass, emailErr, pw, pwClass, pwErr) =
        case formState of
          NotLoggedInValidated email' emailErr' pw' pwErr' ->
            ( email',
              makeFormClass emailErr',
              makeErrMsg "invalidEmailFeedback" emailErr',
              pw',
              makeFormClass pwErr',
              makeErrMsg "invalidPasswordFeedback" pwErr'
            )
          _ ->
            ( "",
              makeFormClass Nothing,
              makeErrMsg "invalidEmailFeedback" Nothing,
              "",
              makeFormClass Nothing,
              makeErrMsg "invalidPasswordFeedback" Nothing
            )
   in layout "Login" (Just Login) $
        div_ [class_ "container-md p-3"] $
          div_ [class_ "row d-flex justify-content-center"] $
            form_ [class_ "col-8", method_ "post", action_ "/login"] $
              fieldset_ $ do
                div_ [class_ "mb-3 form-floating"] $ do
                  input_
                    [ class_ emailClass,
                      type_ "email",
                      name_ "email",
                      id_ "email",
                      required_ "required",
                      value_ email,
                      autofocus_,
                      describedBy_ "invalidEmailFeedback",
                      placeholder_ "hallo@gmail.com"
                    ]
                  label_ [class_ "form-label", for_ "email"] "Email Adresse"
                  emailErr
                div_ [class_ "mb-3 form-floating"] $ do
                  input_
                    [ class_ pwClass,
                      type_ "password",
                      name_ "password",
                      id_ "password",
                      required_ "required",
                      value_ pw,
                      describedBy_ "invalidPasswordFeedback",
                      placeholder_ "foobar"
                    ]
                  label_ [class_ "form-label", for_ "password"] "Passwort"
                  pwErr
                button_ [class_ "btn btn-primary", type_ "submit"] "Einloggen"
  where
    makeFormClass e = "form-control" <> if isJust e then " is-invalid" else ""
    makeErrMsg :: Text -> Maybe Text -> Html ()
    makeErrMsg id text = maybe mempty (div_ [class_ "invalid-feedback", id_ id] . toHtml) text
