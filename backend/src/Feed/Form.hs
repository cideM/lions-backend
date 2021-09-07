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
import Layout (describedBy_)
import Lucid

data State = State
  { welcomeMsgStateDate :: FormFieldState Time.UTCTime,
    welcomeMsgStateMessage :: FormFieldState Text
  }
  deriving (Show)

data Input = Input
  { welcomeMsgInputDate :: Text,
    welcomeMsgInputMessage :: Text
  }
  deriving (Show)

emptyState :: State
emptyState = State NotValidated NotValidated

parse :: Input -> Either State (Text, Time.UTCTime)
parse Input {..} =
  case State (validDate welcomeMsgInputDate) (notEmpty welcomeMsgInputMessage) of
    State (Valid date) (Valid message) ->
      Right (message, date)
    state -> Left state

render :: Input -> State -> Text -> Html ()
render Input {..} State {..} formAction = do
  form_ [method_ "post", class_ "row g-4", action_ formAction] $ do
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
    button_ [class_ "btn btn-primary", type_ "submit"] "Speichern"
