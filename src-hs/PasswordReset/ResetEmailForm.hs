module PasswordReset.ResetEmailForm (form) where

import Data.Text (Text)
import Lucid

-- Form that is shown when users want to reset their passwords
form :: Maybe Text -> Html ()
form errMsg = do
  div_ [class_ "container p-2"] $ do
    div_ [class_ "row d-flex justify-content-center"] $ do
      div_ [class_ "col-md-6"] $ do
        h1_ [class_ "h4 mb-3"] "Passwort Zurücksetzen"
        case errMsg of
          Nothing -> mempty
          Just msg -> p_ [class_ "my-3 alert alert-danger"] $ toHtml msg
        p_ [class_ "my-3 alert alert-secondary"] "Bitte die Email-Adresse eintragen, mit der du beim Lions Club Achern angemeldet bist. Es wird dann ein Link an diese Email-Adresse verschickt, über welchen du dein Passwort ändern kannst."
        form_ [action_ "/passwort/link", method_ "post"] $ do
          label_ [for_ "emailInput", class_ "form-label"] "Email Adresse"
          input_ [required_ "required", class_ "form-control", name_ "email", id_ "emailInput", placeholder_ "platzhalter@email.de"]
          button_ [class_ "mt-3 btn btn-primary", type_ "submit"] "Absenden"
