module WelcomeMessage.Form (render, WelcomeMsgFormState (..)) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Layout (describedBy_)
import Lucid

data WelcomeMsgFormState = Valid | NotValidated | Invalid Text

render :: WelcomeMsgFormState -> Text -> Text -> Html ()
render state formAction currentMsg =
  let messageFieldClass =
        "form-control"
          <> ( case state of
                 NotValidated -> ""
                 Invalid _ -> " is-invalid"
                 Valid -> " is-valid"
             )
      errMsg = case state of
        Invalid e -> Just e
        _ -> Nothing
      successMsg = case state of
        Valid -> Just (div_ [class_ "alert alert-success"] "Neue Nachricht erfolgreich gespeichert")
        _ -> Nothing
   in form_ [method_ "post", action_ formAction] $ do
        fromMaybe mempty successMsg
        fieldset_ [class_ "mb-3"] $ do
          label_ [class_ "form-label", for_ "message"] "Nachricht"
          textarea_
            [ class_ messageFieldClass,
              type_ "textfield",
              name_ "message",
              id_ "message",
              required_ "required",
              autofocus_,
              rows_ "10",
              cols_ "10",
              describedBy_ "invalidMessageFeedback"
            ]
            (toHtml currentMsg)
          maybe mempty (div_ [class_ "invalid-feedback", id_ "invalidMessageFeedback"] . toHtml) errMsg
        button_ [class_ "btn btn-primary", type_ "submit"] "Speichern"
