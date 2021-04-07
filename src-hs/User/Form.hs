{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module User.Form
  ( makeProfile,
    render,
    CanEditRoles (..),
    emptyForm,
    FormInput (..),
  )
where

import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Data.Time.Format as TF
import Layout (describedBy_)
import Locale (german)
import Lucid
import qualified Text.Email.Validate as Email
import User.Domain (Role (..), UserProfile (..))

data FormFieldState a = NotValidated | Valid a | Invalid Text deriving (Show)

data EditFormState = EditFormState
  { formStateEmail :: FormFieldState Email.EmailAddress,
    formStateBirthday :: FormFieldState (Maybe Time.Day),
    formStateBirthdayPartner :: FormFieldState (Maybe Time.Day)
  }
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
      let (className, errMsg) = processField formStateBirthday
      label_ [class_ "form-label", for_ "inputBirthday"] "Geburtstag"
      input_
        [ class_ className,
          value_ inputBirthday,
          type_ "text",
          placeholder_ "09.02.1951",
          describedBy_ "validationBirthdayFeedback bdayHelp",
          id_ "inputBirthday",
          pattern_ "\\d{2}.\\d{2}.\\d{4}",
          name_ "inputBirthday"
        ]
      div_ [id_ "bdayHelp", class_ "form-text"] "Tag.Monat.Jahr -> 31.01.1990"
      maybe mempty (div_ [id_ "validationBirthdayFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-6"] $ do
      let (className, errMsg) = processField formStateBirthdayPartner
      label_ [class_ "form-label", for_ "inputBirthdayPartner"] "Geburtstag Partner"
      input_
        [ class_ className,
          value_ inputBirthdayPartner,
          type_ "text",
          placeholder_ "09.02.1951",
          describedBy_ "validationBirthdayPartnerFeedback bdayPHelp",
          id_ "inputBirthdayPartner",
          pattern_ "\\d{2}.\\d{2}.\\d{4}",
          name_ "inputBirthdayPartner"
        ]
      div_ [id_ "bdayPHelp", class_ "form-text"] "Tag.Monat.Jahr -> 31.01.1990"
      maybe mempty (div_ [id_ "validationBirthdayPartnerFeedback", class_ "invalid-feedback"] . toHtml) errMsg
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
  where
    classValidated :: [(Text, Bool)] -> Text
    classValidated pairs = Text.intercalate " " [className | (className, predicate) <- pairs, predicate]

    processField :: FormFieldState a -> (Text, Maybe Text)
    processField field = (makeInputClass field, getErrorMsg field)

    getErrorMsg (Invalid msg) = Just msg
    getErrorMsg _ = Nothing

    fieldIsValid (Valid _) = True
    fieldIsValid _ = False

    makeInputClass :: FormFieldState a -> Text
    makeInputClass field = classValidated [("form-control", True), ("is-invalid", isJust $ getErrorMsg field), ("is-valid", fieldIsValid field)]

emptyForm :: EditFormState
emptyForm =
  EditFormState
    { formStateEmail = NotValidated,
      formStateBirthday = NotValidated,
      formStateBirthdayPartner = NotValidated
    }

data FormInput = FormInput
  { inputEmail :: Text,
    inputBirthday :: Text,
    inputBirthdayPartner :: Text,
    inputIsAdmin :: Bool,
    inputIsBoard :: Bool,
    inputIsPresident :: Bool,
    inputAddress :: Text,
    inputFirstName :: Text,
    inputFirstNamePartner :: Text,
    inputLastName :: Text,
    inputLastNamePartner :: Text,
    inputMobile :: Text,
    inputLandline :: Text
  }
  deriving (Show)

makeProfile :: FormInput -> Either EditFormState (UserProfile, [Role])
makeProfile FormInput {..} =
  case EditFormState (validateEmail inputEmail) (validateDate inputBirthday) (validateDate inputBirthdayPartner) of
    (EditFormState (Valid email) (Valid bday) (Valid bdayp)) ->
      Right
        ( UserProfile
            email
            (Just inputFirstName)
            (Just inputLastName)
            (Just inputAddress)
            (Just inputMobile)
            (Just inputLandline)
            bday
            (Just inputFirstNamePartner)
            (Just inputLastNamePartner)
            bdayp,
          catMaybes
            [ if inputIsAdmin then Just Admin else Nothing,
              if inputIsBoard then Just Board else Nothing,
              if inputIsPresident then Just President else Nothing,
              Just User
            ]
        )
    state -> Left state
  where
    validateEmail "" = Invalid "Email darf nicht leer sein"
    validateEmail email =
      case Email.emailAddress (encodeUtf8 email) of
        Just addr -> Valid addr
        Nothing -> Invalid "Falsches Format"

    validateDate "" = Valid Nothing
    validateDate datestr =
      case TF.parseTimeM True german "%d.%m.%Y" $ Text.unpack datestr of
        Nothing -> Invalid "Falsches Format"
        date -> Valid date
