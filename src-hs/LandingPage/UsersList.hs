{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LandingPage.UsersList (render, UserGroupToShow (..)) where

import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Layout (ariaLabel_)
import Lucid
import TextShow
import User.Domain (Role (..), UserId, UserProfile (..), showEmail)

data UserGroupToShow = All | Some Role deriving (Eq)

instance Show UserGroupToShow where
  show All = "all"
  show (Some r) = show r

render :: [([Role], (UserId, UserProfile))] -> Bool -> UserGroupToShow -> Html ()
render users userIsAdmin activeGroup = do
  div_ [class_ "d-flex justify-content-between align-items-center mb-3 flex-wrap"] $ do
    h1_ [class_ "card-title fs-3"] "Mitgliederliste"
    form_ [method_ "get", action_ "/nutzer", class_ "d-flex"] $ do
      select_ [name_ "userselect", class_ "form-select form-select-sm me-1", ariaLabel_ "Nutzergruppe auswählen"] $ do
        mapM_
          (\(role, label) -> option_ (value_ (Text.pack $ show role) : [selected_ "selected" | role == activeGroup]) label)
          [ (All, "Alle anzeigen"),
            (Some User, "Nur Nutzer"),
            (Some Admin, "Nur Administratoren"),
            (Some Board, "Nur Vorstand"),
            (Some President, "Nur Präsident")
          ]
      button_ [class_ "btn btn-sm btn-secondary", type_ "submit"] "Ok"
  ul_ [class_ "list-group mb-3", id_ "userslist"] $
    mapM_
      ( \(userRoles, (userId, p)) -> do
          let name = fromMaybe "" (userFirstName p) <> " " <> fromMaybe "" (userLastName p)
          li_ [class_ "p-0 list-group-item d-flex align-items-center", data_ "email" (showEmail $ userEmail p)] $ do
            a_ [class_ "flex-grow-1 py-2 px-3 list-group-item-action text-decoration-none text-body", href_ ("/nutzer/" <> showt userId)] $ do
              div_ [class_ "me-auto d-grid gap-2"] $ do
                unless (Text.null name) $
                  p_ [class_ "fw-bold m-0"] $ toHtml name
                p_ [class_ "m-0"] $ toHtml (showEmail $ userEmail p)
                div_ [class_ "text-muted"] $ toHtml . displayBadges $ userRoles
      )
      users
  when userIsAdmin $ a_ [class_ "link-primary", href_ "/nutzer/neu"] "Neues Mitglied hinzufügen"
  where
    displayBadges = Text.intercalate ", " . map showBadge . filter dropUser
    dropUser User = False
    dropUser _ = True
    showBadge President = "Präsident"
    showBadge Board = "Vorstand"
    showBadge Admin = "Administrator"
    showBadge User = "Nutzer"
