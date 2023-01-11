module Events.Event.Form
  ( render,
    FormState (..),
    FormInput (..),
    emptyForm,
    emptyState,
    makeEvent,
  )
where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Time as Time
import Form (FormFieldState (..), notEmpty, validDate)
import Layout (describedBy_)
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

makeEvent :: FormInput -> Either FormState (Text, Time.UTCTime, Bool, Text, Text)
makeEvent input =
  case FormState
    (notEmpty $ createEventInputTitle input)
    (validDate $ createEventInputDate input)
    (notEmpty $ createEventInputLocation input)
    (Valid $ createEventInputDescription input) of
    FormState (Valid title) (Valid date) (Valid location) (Valid description) ->
      Right $ (title, date, (createEventInputFamilyAllowed input), description, location)
    state -> Left state

render :: Text -> Text -> [(Text, Bool)] -> FormInput -> FormState -> Html ()
render btnLabel action checkboxes FormInput {..} FormState {..} = do
  form_ [class_ "row g-4", action_ action, method_ "post", enctype_ "multipart/form-data"] $ do
    div_ [class_ "col-md-6"] $ do
      let (className, errMsg) = case createEventStateTitle of
            NotValidated -> ("", Nothing)
            Valid _ -> ("is-valid", Nothing)
            Invalid msg -> ("is-invalid", Just msg)
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
      let (className, errMsg) = case createEventStateLocation of
            NotValidated -> ("", Nothing)
            Valid _ -> ("is-valid", Nothing)
            Invalid msg -> ("is-invalid", Just msg)
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
      let (className, errMsg) = case createEventStateDate of
            NotValidated -> ("", Nothing)
            Valid _ -> ("is-valid", Nothing)
            Invalid msg -> ("is-invalid", Just msg)
      label_ [for_ "eventDateInput", class_ "form-label"] "Datum"
      input_
        [ type_ "text",
          id_ "eventDateInput",
          value_ createEventInputDate,
          class_ className,
          required_ "required",
          name_ "eventDateInput",
          pattern_ "\\d{2}.\\d{2}.\\d{4} \\d{2}:\\d{2}",
          describedBy_ "eventDateHelp eventDateFeedback"
        ]
      div_ [id_ "eventDateHelp", class_ "form-text"] "Bitte als Format '12.01.2022 15:00' verwenden."
      maybe mempty (div_ [id_ "eventDateFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-12 d-flex flex-column"] $ do
      let (className, errMsg) = case createEventStateDescription of
            NotValidated -> ("", Nothing)
            Valid _ -> ("is-valid", Nothing)
            Invalid msg -> ("is-invalid", Just msg)
      label_ [for_ "eventDescriptionInput", class_ "form-label"] "Beschreibung"
      textarea_
        [ class_ className,
          type_ "textfield",
          name_ "eventDescriptionInput",
          id_ "eventDescriptionInput",
          autofocus_,
          rows_ "10",
          describedBy_ "eventDescriptionFeedback",
          cols_ "10"
        ]
        $ toHtml createEventInputDescription
      maybe mempty (div_ [id_ "eventDescriptionFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-6"] $ do
      label_ [for_ "eventAttachmentsInput", class_ "form-label"] "Dateien"
      input_
        [ type_ "file",
          multiple_ "multiple",
          class_ "form-control",
          id_ "eventAttachmentsInput",
          name_ "eventAttachmentsInput"
        ]
    div_ [class_ "col-md-6"] $
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
    div_ [class_ "col-md-12"] $
      when (length checkboxes > 0) $ do
        mapM_
          ( \(name, checked) ->
              div_ [class_ "form-check"] $ do
                input_ $
                  [ type_ "checkbox",
                    class_ "form-check-input",
                    id_ name,
                    name_ "newFileCheckbox",
                    value_ name
                  ]
                    ++ [checked_ | checked]
                label_ [for_ name, class_ "form-check-label"] $ toHtml name
          )
          checkboxes
    div_ [class_ "col-md-4"] $
      button_ [class_ "btn btn-primary", type_ "submit"] $
        toHtml btnLabel
