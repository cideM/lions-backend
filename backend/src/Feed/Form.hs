module Feed.Form
  ( render,
    State (..),
    Input (..),
    emptyState,
    parse,
  )
where

import Data.Text (Text)
import qualified Data.Time as Time
import Form (FormFieldState (..), notEmpty, processField, validDate)
import Control.Monad (when)
import Layout (describedBy_)
import Lucid

data State = State
  { welcomeMsgStateDate :: FormFieldState Time.UTCTime,
    welcomeMsgStateMessage :: FormFieldState Text
  }
  deriving (Show)

data Input = Input
  { welcomeMsgInputDate :: Text,
    welcomeMsgInputMessage :: Text,
    welcomeMsgInputFiles :: [(Text, Bool)]
  }
  deriving (Show)

emptyState :: State
emptyState = State NotValidated NotValidated

-- TODO: This should return a message?
-- TODO: Should this be part of Message? Instead of "Input" it could also just
-- be a function accepting every field as input. Then it's a bit more general.
-- On the other hand, what if sometimes you parse the date from a number and
-- another time you parse it from text? I think a newtype approach might work,
-- where each field is a newtype which could  then be parsed from multiple
-- sources? Or you have multiple places that all have some logic for creating a
-- valid Message?
parse :: Input -> Either State (Text, Time.UTCTime)
parse Input {..} =
  case State (validDate welcomeMsgInputDate) (notEmpty welcomeMsgInputMessage) of
    State (Valid date) (Valid message) ->
      Right (message, date)
    state -> Left state

render :: Input -> State -> Text -> Html ()
render Input {..} State {..} formAction = do
  form_ [class_ "row g-4", action_ formAction, method_ "post", enctype_ "multipart/form-data"] $ do
    div_ [class_ "col-12"] $ do
      let (className, errMsg) = processField welcomeMsgStateDate
      label_ [class_ "form-label", for_ "date"] "Datum"
      input_
        [ class_ className,
          type_ "text",
          name_ "date",
          pattern_ "\\d{2}.\\d{2}.\\d{4} \\d{2}:\\d{2}",
          value_ welcomeMsgInputDate,
          id_ "date",
          required_ "required",
          describedBy_ "invalidDateFeedback"
        ]
      maybe mempty (div_ [class_ "invalid-feedback", id_ "invalidDateFeedback"] . toHtml) errMsg
    div_ [class_ "col-12"] $ do
      let (className, errMsg) = processField welcomeMsgStateMessage
      label_ [class_ "form-label", for_ "message"] "Nachricht"
      textarea_
        [ class_ className,
          type_ "textfield",
          name_ "message",
          id_ "message",
          required_ "required",
          autofocus_,
          rows_ "10",
          cols_ "10",
          describedBy_ "invalidMessageFeedback"
        ]
        (toHtml welcomeMsgInputMessage)
      maybe mempty (div_ [class_ "invalid-feedback", id_ "invalidMessageFeedback"] . toHtml) errMsg
    div_ [class_ "col-md-12"] $
      when (length welcomeMsgInputFiles > 0) $ do
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
          welcomeMsgInputFiles
    div_ [class_ "col-md-6"] $ do
      label_ [for_ "attachments", class_ "form-label"] "Dateien"
      input_
        [ type_ "file",
          multiple_ "multiple",
          class_ "form-control",
          id_ "attachments",
          name_ "attachments"
        ]
    button_ [class_ "btn btn-primary", type_ "submit"] "Speichern"
