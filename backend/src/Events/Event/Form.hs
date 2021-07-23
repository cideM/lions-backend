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
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import Events.Event.Event (Event (..))
import Form (FormFieldState (..), notEmpty, processField, validDate)
import Layout (describedBy_)
import Lucid

-- TODO: Rename create because it's both edit and create
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
    createEventInputFamilyAllowed :: Bool,
    createEventInputCheckboxes :: [(Text, Bool)],
    createEventInputFilesHidden :: [ByteString]
  }
  deriving (Show)

emptyForm :: FormInput
emptyForm =
  FormInput
    { createEventInputTitle = "",
      createEventInputDate = "",
      createEventInputLocation = "",
      createEventInputDescription = "",
      createEventInputFamilyAllowed = False,
      createEventInputCheckboxes = [],
      createEventInputFilesHidden = []
    }

emptyState :: FormState
emptyState = FormState NotValidated NotValidated NotValidated NotValidated

makeEvent :: FormInput -> Either FormState (Event Text)
makeEvent FormInput {..} =
  case FormState
    (notEmpty createEventInputTitle)
    (validDate createEventInputDate)
    (notEmpty createEventInputLocation)
    (notEmpty createEventInputDescription) of
    FormState (Valid title) (Valid date) (Valid location) (Valid description) ->
      let filesToKeep = [name | (name, checked) <- createEventInputCheckboxes, checked]
       in Right $
            Event
              title
              date
              createEventInputFamilyAllowed
              description
              location
              []
              filesToKeep
    state -> Left state

render :: Text -> Text -> FormInput -> FormState -> Html ()
render btnLabel action FormInput {..} FormState {..} = do
  form_ [class_ "row g-4", action_ action, method_ "post", enctype_ "multipart/form-data"] $ do
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
      when (length createEventInputCheckboxes > 0) $ do
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
          createEventInputCheckboxes
        mapM_
          ( \value ->
              input_ $
                [ type_ "hidden",
                  name_ "allFiles",
                  value_ $ decodeUtf8 value
                ]
          )
          createEventInputFilesHidden
    div_ [class_ "col-md-4"] $
      button_ [class_ "btn btn-primary", type_ "submit"] $ toHtml btnLabel
