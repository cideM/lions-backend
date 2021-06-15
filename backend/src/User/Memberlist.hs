module User.Memberlist (render, UserGroupToShow (..)) where

import Control.Monad (unless)
import Data.List.NonEmpty (toList)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Layout (ariaLabel_)
import Lucid
import User.Types (Role (..), UserEmail (..), UserId (..), UserProfile (..), showEmail)

data UserGroupToShow = All | Some Role deriving (Eq)

instance Show UserGroupToShow where
  show All = "all"
  show (Some r) = show r

render :: [UserProfile] -> UserGroupToShow -> Html ()
render users activeGroup = do
  div_ [class_ "d-flex justify-content-between align-items-center mb-3 flex-wrap"] $ do
    form_ [method_ "get", action_ "/nutzer", class_ "d-flex mb-2"] $ do
      select_ [name_ "userselect", class_ "form-select form-select-sm me-1", ariaLabel_ "Nutzergruppe auswählen"] $ do
        mapM_
          (\(role, label) -> option_ (value_ (Text.pack $ show role) : [selected_ "selected" | role == activeGroup]) label)
          [ (All, "Alle anzeigen"),
            (Some User, "Nur Nutzer"),
            (Some Admin, "Nur Administratoren"),
            (Some Passive, "Nur Passiv"),
            (Some Board, "Nur Vorstand"),
            (Some President, "Nur Präsident")
          ]
      button_ [class_ "btn btn-sm btn-secondary", type_ "submit"] "Ok"
    a_ [href_ "", class_ "btn btn-primary btn-sm disabled", role_ "button", id_ "email-button"] $ "Email abschicken"
  table_ [class_ "table mb-3", id_ "userslist"] $ do
    thead_ [] $
      tr_ [class_ "align-middle"] $ do
        th_ [scope_ "col", class_ "text-center"] "Profil"
        th_ [scope_ "col mw-50"] "Email"
        th_ [scope_ "col", class_ "d-none d-lg-table-cell"] "Name"
        th_ [scope_ "col", class_ "d-none d-lg-table-cell"] "Status"
        th_ [scope_ "col", class_ "text-center"] $ envelopeSvg
    tbody_ [] $
      mapM_
        ( \UserProfile {..} -> do
            let name = fromMaybe "" userFirstName <> " " <> fromMaybe "" userLastName
                (UserEmail email) = userEmail
                (UserId uid) = userId
            tr_ [] $ do
              td_ [class_ "text-center align-middle"] $
                a_ [href_ ("/nutzer/" <> (Text.pack $ show uid))] personCircleSvg
              td_ [] $ a_ [href_ [i|mailto:#{showEmail email}|], class_ "text-break"] . toHtml $ showEmail email
              td_ [class_ "d-none d-lg-table-cell"] $ unless (Text.null $ Text.strip name) $ p_ [class_ "fw-bold m-0"] $ toHtml name
              td_ [class_ "text-muted d-none d-lg-table-cell"] $ toHtml . displayBadges $ toList userRoles
              td_ [class_ "text-center align-middle"] $
                input_ [type_ "checkbox", value_ "", data_ "email" (showEmail email)]
        )
        users
  where
    envelopeSvg =
      toHtmlRaw
        ( [i|
      <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" fill="currentColor" role="button" class="bi text-muted bi-envelope" id="toggle-email-button" viewBox="0 0 16 16">
        <path d="M.05 3.555A2 2 0 0 1 2 2h12a2 2 0 0 1 1.95 1.555L8 8.414.05 3.555zM0 4.697v7.104l5.803-3.558L0 4.697zM6.761 8.83l-6.57 4.027A2 2 0 0 0 2 14h12a2 2 0 0 0 1.808-1.144l-6.57-4.027L8 9.586l-1.239-.757zm3.436-.586L16 11.801V4.697l-5.803 3.546z"/>
      </svg>
      |] ::
            Text
        )
    personCircleSvg =
      toHtmlRaw
        ( [i|
      <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" fill="currentColor" class="text-muted bi bi-person-circle" viewBox="0 0 16 16">
        <path d="M11 6a3 3 0 1 1-6 0 3 3 0 0 1 6 0z"/>
        <path fill-rule="evenodd" d="M0 8a8 8 0 1 1 16 0A8 8 0 0 1 0 8zm8-7a7 7 0 0 0-5.468 11.37C3.242 11.226 4.805 10 8 10s4.757 1.225 5.468 2.37A7 7 0 0 0 8 1z"/>
      </svg>
      |] ::
            Text
        )
    displayBadges = Text.intercalate ", " . map showBadge . filter dropUser
    dropUser User = False
    dropUser _ = True
    showBadge President = "Präsident"
    showBadge Passive = "Passiv"
    showBadge Board = "Vorstand"
    showBadge Admin = "Administrator"
    showBadge User = "Nutzer"
