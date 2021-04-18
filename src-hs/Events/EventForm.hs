{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Events.EventForm
  ( render,
    FormState (..),
    FormInput (..),
    emptyForm,
    emptyState,
    makeEvent,
  )
where

import Data.Text (Text)
-- import qualified Data.Text as Text

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Format as TF
import Events.Domain (EventCreate (..))
import Form.Form (FormFieldState (..), processField)
import Layout (describedBy_)
import Locale (german)
import Lucid

data FormState = FormState
  { createEventStateTitle :: FormFieldState Text,
    createEventStateDate :: FormFieldState Time.UTCTime,
    createEventStateLocation :: FormFieldState Text,
    createEventStateDescription :: FormFieldState Text
  }
  deriving (Show)

data FormInput = FormInput
  { createEventInputTitle :: Text,
    createEventInputDate :: Text,
    createEventInputLocation :: Text,
    createEventInputDescription :: Text,
    createEventInputFamilyAllowed :: Bool
  }
  deriving (Show)

emptyForm :: FormInput
emptyForm =
  FormInput
    { createEventInputTitle = "",
      createEventInputDate = "",
      createEventInputLocation = "",
      createEventInputDescription = "",
      createEventInputFamilyAllowed = False
    }

emptyState :: FormState
emptyState = FormState NotValidated NotValidated NotValidated NotValidated

makeEvent :: FormInput -> Either FormState EventCreate
makeEvent FormInput {..} =
  case FormState
    (notEmpty createEventInputTitle)
    (validDate createEventInputDate)
    (notEmpty createEventInputLocation)
    (notEmpty createEventInputDescription) of
    FormState (Valid title) (Valid date) (Valid location) (Valid description) ->
      Right $ EventCreate title date createEventInputFamilyAllowed location description
    state -> Left state
  where
    notEmpty "" = Invalid "Feld darf nicht leer sein"
    notEmpty v = Valid v

    validDate "" = Invalid "Feld darf nicht leer sein"
    validDate datestr =
      case TF.parseTimeM True german "%d.%m.%Y %R" $ Text.unpack datestr of
        Nothing -> Invalid "Falsches Format"
        Just date -> Valid date

render :: FormInput -> FormState -> Html ()
render FormInput {..} FormState {..} = do
  form_ [class_ "row g-4", action_ "/veranstaltungen/neu", method_ "post"] $ do
    div_ [class_ "col-md-6"] $ do
      let (className, errMsg) = processField createEventStateTitle
      label_ [for_ "eventTitleInput", class_ "form-label"] "Name"
      input_
        [ type_ "text",
          id_ "eventTitleInput",
          required_ "required",
          name_ "eventTitleInput",
          class_ className,
          describedBy_ "eventTitleFeedback",
          value_ createEventInputTitle
        ]
      maybe mempty (div_ [id_ "eventTitleFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-6"] $ do
      let (className, errMsg) = processField createEventStateLocation
      label_ [for_ "eventLocationInput", class_ "form-label"] "Ort"
      input_
        [ type_ "text",
          class_ className,
          id_ "eventLocationInput",
          value_ createEventInputLocation,
          required_ "required",
          describedBy_ "eventLocationFeedback",
          name_ "eventLocationInput"
        ]
      maybe mempty (div_ [id_ "eventLocationFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-6 d-flex flex-column"] $ do
      let (className, errMsg) = processField createEventStateDate
      label_ [for_ "eventDateInput", class_ "form-label"] "Datum"
      input_
        [ type_ "text",
          id_ "eventDateInput",
          value_ createEventInputLocation,
          class_ className,
          required_ "required",
          name_ "eventDateInput",
          pattern_ "\\d{2}.\\d{2}.\\d{4} \\d{2}:\\d{2}",
          describedBy_ "eventDateHelp eventDateFeedback"
        ]
      div_ [id_ "eventDateHelp", class_ "form-text"] "Bitte als Format '12.01.2022 15:00' verwenden."
      maybe mempty (div_ [id_ "eventDateFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-12 d-flex flex-column"] $ do
      let (className, errMsg) = processField createEventStateDescription
      label_ [for_ "eventDescriptionInput", class_ "form-label"] "Beschreibung"
      textarea_
        [ class_ className,
          type_ "textfield",
          name_ "eventDescriptionInput",
          id_ "eventDescriptionInput",
          required_ "required",
          autofocus_,
          rows_ "10",
          describedBy_ "eventDescriptionFeedback",
          cols_ "10"
        ]
        $ toHtml createEventInputDescription
      maybe mempty (div_ [id_ "eventDescriptionFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-12"] $
      div_ [class_ "form-check"] $ do
        input_
          ( [ class_ "form-check-input",
              type_ "checkbox",
              id_ "eventFamAllowedInput",
              name_ "eventFamAllowedInput"
            ]
              ++ [checked_ | createEventInputFamilyAllowed]
          )
        label_ [class_ "form-check-label", for_ "eventFamAllowedInput"] "Mit Familie?"
    div_ [class_ "col-md-4"] $
      button_ [class_ "btn btn-primary", type_ "submit"] "Speichern"
