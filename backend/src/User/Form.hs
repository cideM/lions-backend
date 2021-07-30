module User.Form
  ( makeProfile,
    render,
    CanEditRoles (..),
    emptyForm,
    FormInput (..),
  )
where

import Control.Exception.Safe
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Form (FormFieldState (..), processField)
import Layout (describedBy_)
import Lucid
import qualified Text.Email.Validate as Email
import User.Role.Role (Role (..))
import qualified User.User as User
import qualified User.Email as UserEmail

newtype EditFormState = EditFormState
  {formStateEmail :: FormFieldState Email.EmailAddress}
  deriving (Show)

newtype CanEditRoles = CanEditRoles Bool

render :: CanEditRoles -> Text -> Text -> FormInput -> EditFormState -> Html ()
render (CanEditRoles canEditRoles) btnlabel action FormInput {..} EditFormState {..} =
  form_ [class_ "row g-3", method_ "POST", action_ action] $ do
    div_ [class_ "col-12"] $ do
      let (className, errMsg) = processField formStateEmail
      label_ [class_ "form-label", for_ "inputEmail"] "Email"
      input_
        [ class_ className,
          type_ "email",
          id_ "inputEmail",
          placeholder_ "test@adresse.de",
          required_ "required",
          describedBy_ "validationEmailFeedback",
          value_ inputEmail,
          name_ "inputEmail"
        ]
      maybe mempty (div_ [id_ "validationEmailFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-12"] $ do
      label_ [class_ "form-label", for_ "inputAddress"] "Adresse"
      textarea_ [rows_ "4", cols_ "20", class_ "form-control", id_ "inputAddress", name_ "inputAddress"] (toHtml inputAddress)
    div_ [class_ "col-md-6"] $ do
      label_ [class_ "form-label", for_ "inputFirstName"] "Vorname"
      input_ [class_ "form-control", value_ inputFirstName, type_ "text", id_ "inputFirstName", name_ "inputFirstName"]
    div_ [class_ "col-md-6"] $ do
      label_ [class_ "form-label", for_ "inputLastName"] "Nachname"
      input_ [value_ inputLastName, class_ "form-control", type_ "text", id_ "inputLastName", name_ "inputLastName"]
    div_ [class_ "col-md-6"] $ do
      label_ [class_ "form-label", for_ "inputFirstNamePartner"] "Vorname Partner"
      input_ [value_ inputFirstNamePartner, class_ "form-control", type_ "text", id_ "inputFirstNamePartner", name_ "inputFirstNamePartner"]
    div_ [class_ "col-md-6"] $ do
      label_ [class_ "form-label", for_ "inputLastNamePartner"] "Nachname Partner"
      input_ [value_ inputLastNamePartner, class_ "form-control", type_ "text", id_ "inputLastNamePartner", name_ "inputLastNamePartner"]
    div_ [class_ "col-md-6"] $ do
      label_ [class_ "form-label", for_ "inputBirthday"] "Geburtstag"
      input_
        [ class_ "form-control",
          value_ inputBirthday,
          type_ "text",
          placeholder_ "09.02.1951",
          id_ "inputBirthday",
          name_ "inputBirthday"
        ]
    div_ [class_ "col-md-6"] $ do
      label_ [class_ "form-label", for_ "inputBirthdayPartner"] "Geburtstag Partner"
      input_
        [ class_ "form-control",
          value_ inputBirthdayPartner,
          type_ "text",
          placeholder_ "09.02.1951",
          id_ "inputBirthdayPartner",
          name_ "inputBirthdayPartner"
        ]
    div_ [class_ "col-md-6"] $ do
      label_ [class_ "form-label", for_ "inputMobile"] "Handynummer"
      input_ [value_ inputMobile, class_ "form-control", type_ "tel", pattern_ "[0-9-+]*", describedBy_ "mobileHelp", id_ "inputMobile", name_ "inputMobile"]
      div_ [id_ "mobileHelp", class_ "form-text"] "Nur Zahlen, - und +"
    div_ [class_ "col-md-6"] $ do
      label_ [class_ "form-label", for_ "inputLandline"] "Festnetz"
      input_ [value_ inputLandline, class_ "form-control", type_ "tel", pattern_ "[0-9-+]*", describedBy_ "landlineHelp", id_ "inputLandline", name_ "inputLandline"]
      div_ [id_ "landlineHelp", class_ "form-text"] "Nur Zahlen, - und +"
    fieldset_ [class_ "col-12"] $ do
      legend_ [class_ "col-form-label col-12 pt-0"] "Nutzergruppen"
      div_ [class_ "form-check col-12"] $ do
        input_ $
          [ class_ "form-check-input",
            type_ "checkbox",
            id_ "inputIsAdmin",
            name_ "inputIsAdmin"
          ]
            ++ [checked_ | inputIsAdmin]
            ++ [disabled_ "" | not canEditRoles]
        label_ [class_ "form-check-label", for_ "inputIsAdmin"] "Administrator"
      div_ [class_ "form-check col-12"] $ do
        input_ $
          [ class_ "form-check-input",
            type_ "checkbox",
            id_ "inputIsBoard",
            name_ "inputIsBoard"
          ]
            ++ [checked_ | inputIsBoard]
            ++ [disabled_ "" | not canEditRoles]
        label_ [class_ "form-check-label", for_ "inputIsBoard"] "Vorstand"
      div_ [class_ "form-check col-12"] $ do
        input_ $
          [ class_ "form-check-input",
            type_ "checkbox",
            id_ "inputIsPresident",
            name_ "inputIsPresident"
          ]
            ++ [checked_ | inputIsPresident]
            ++ [disabled_ "" | not canEditRoles]
        label_ [class_ "form-check-label", for_ "inputIsPresident"] "Präsident"
      div_ [class_ "form-check col-12"] $ do
        input_ $
          [ class_ "form-check-input",
            type_ "checkbox",
            id_ "inputIsPassive",
            name_ "inputIsPassive"
          ]
            ++ [checked_ | inputIsPassive]
            ++ [disabled_ "" | not canEditRoles]
        label_ [class_ "form-check-label", for_ "inputIsPassive"] "Passiv"
      div_ [class_ "form-check col-12"] $ do
        input_
          [ class_ "form-check-input",
            type_ "checkbox",
            id_ "inputIsUser",
            checked_,
            name_ "inputIsUser",
            disabled_ "disabled"
          ]
        input_ [type_ "hidden", checked_, name_ "inputIsUser"]
        label_ [class_ "form-check-label", for_ "inputIsUser"] "Nutzer"
    button_ [type_ "submit", class_ "btn btn-primary"] $ toHtml btnlabel

emptyForm :: EditFormState
emptyForm = EditFormState {formStateEmail = NotValidated}

data FormInput = FormInput
  { inputEmail :: Text,
    inputBirthday :: Text,
    inputBirthdayPartner :: Text,
    inputIsAdmin :: Bool,
    inputIsBoard :: Bool,
    inputIsPresident :: Bool,
    inputIsPassive :: Bool,
    inputAddress :: Text,
    inputFirstName :: Text,
    inputFirstNamePartner :: Text,
    inputLastName :: Text,
    inputLastNamePartner :: Text,
    inputMobile :: Text,
    inputLandline :: Text
  }
  deriving (Show)

makeProfile :: FormInput -> IO (Either EditFormState User.Profile)
makeProfile FormInput {..} =
  case EditFormState (validateEmail inputEmail) of
    (EditFormState (Valid email)) ->
      let roles =
            NE.nonEmpty $
              catMaybes
                [ if inputIsAdmin then Just Admin else Nothing,
                  if inputIsBoard then Just Board else Nothing,
                  if inputIsPresident then Just President else Nothing,
                  if inputIsPassive then Just Passive else Nothing,
                  Just User
                ]
       in case roles of
            Nothing -> throwString "empty roles in user creation form"
            Just roles' ->
              return . Right $
                User.Profile
                  (UserEmail.Email email)
                  (Just inputFirstName)
                  (Just inputLastName)
                  (Just inputAddress)
                  (Just inputMobile)
                  (Just inputLandline)
                  (Just inputBirthday)
                  (Just inputFirstNamePartner)
                  (Just inputLastNamePartner)
                  (Just inputBirthdayPartner)
                  roles'
    state -> return $ Left state
  where
    validateEmail "" = Invalid "Email darf nicht leer sein"
    validateEmail email =
      case Email.emailAddress (encodeUtf8 email) of
        Just addr -> Valid addr
        Nothing -> Invalid "Falsches Format"
