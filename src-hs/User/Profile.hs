{-# LANGUAGE OverloadedStrings #-}

module User.Profile (render, CanDelete(..), CanEdit(..)) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Locale (german)
import Lucid
import TextShow
import User.Domain (Role (..), UserId (..), UserProfile (..), showEmail)

newtype CanDelete = CanDelete Bool

newtype CanEdit = CanEdit Bool

render :: [Role] -> UserId -> UserProfile -> CanDelete -> CanEdit -> Html ()
render roles userId profile (CanDelete canDelete) (CanEdit canEdit) = do
  div_ [class_ "card"] $ do
    div_ [class_ "card-header"] "Nutzerprofil"
    div_ [class_ "card-body"] $ do
      h1_ [class_ "card-title fs-3"] $ do
        toHtml (fromMaybe "" (userFirstName profile) <> " " <> fromMaybe "" (userLastName profile))
      div_ [class_ "text-muted mb-4"] $ toHtml $ displayBadges roles
      div_ [class_ "row"] $ do
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Email"
          p_ [class_ "fs-5"] $ a_ [href_ $ "mailto:" <> showEmail (userEmail profile)] $ toHtml $ showEmail (userEmail profile)
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Addresse"
          p_ [class_ "fs-5", style_ "whitespace: pre"] $ toHtml' (userAddress profile)
      div_ [class_ "row"] $ do
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Handynummer"
          p_ [class_ "fs-5"] $ toHtml' (userMobilePhoneNr profile)
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Festnetz"
          p_ [class_ "fs-5"] $ toHtml' (userLandlineNr profile)
      div_ [class_ "row"] $ do
        let formatted = Text.pack . Time.formatTime german "%A, %d. %B %Y" <$> userBirthday profile
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Geburtstag"
          p_ [class_ "fs-5"] $ toHtml' formatted
      div_ [class_ "row"] $ do
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Name des Partners"
          p_ [class_ "fs-5"] $ toHtml (fromMaybe "" (userFirstNamePartner profile) <> " " <> fromMaybe "" (userLastNamePartner profile))
        div_ [class_ "mb-2 col-md-6"] $ do
          let formatted = Text.pack . Time.formatTime german "%A, %d. %B %Y" <$> userBirthdayPartner profile
          small_ [class_ "text-muted"] "Geburtstag des Partners"
          p_ [class_ "fs-5"] $ toHtml' formatted
    div_ [class_ "card-footer d-flex d-grid"] $ do
      when canDelete $ do
        form_ [action_ $ "/nutzer/" <> showt userId <> "/löschen", method_ "get"] $
          button_ [class_ "btn btn-danger me-4", type_ "submit"] "Nutzer löschen"
      when canEdit $ do
        form_ [action_ $ "/nutzer/" <> showt userId <> "/editieren", method_ "get"] $
          button_ [class_ "btn btn-secondary me-4", type_ "submit"] "Nutzer editieren"
  where
    toHtml' = toHtml . fromMaybe ""
    displayBadges = Text.intercalate ", " . map showBadge . filter dropUser
    dropUser User = False
    dropUser _ = True
    showBadge President = "Präsident"
    showBadge Board = "Vorstand"
    showBadge Admin = "Administrator"
    showBadge User = "Nutzer"
