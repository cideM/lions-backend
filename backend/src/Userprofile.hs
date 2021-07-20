module Userprofile (get) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import Layout (ActiveNavLink (..), LayoutStub (..))
import Lucid
import Session.Auth (Authenticated)
import qualified Session.Auth as Session
import User.DB (getUser)
import User.Types
  ( Role (..),
    UserEmail (..),
    UserId (..),
    UserProfile (..),
    showEmail,
  )
import qualified User.Types as User

newtype CanDelete = CanDelete Bool

newtype CanEdit = CanEdit Bool

render :: UserProfile -> CanDelete -> CanEdit -> Html ()
render UserProfile {..} (CanDelete canDelete) (CanEdit canEdit) = do
  let (UserId uid) = userId
  div_ [class_ "card"] $ do
    div_ [class_ "card-header"] "Nutzerprofil"
    div_ [class_ "card-body"] $ do
      let fullName = fromMaybe "" userFirstName <> " " <> fromMaybe "" userLastName
          -- Every user has role "User"
          hasDisplayRules = length userRoles > 2
      unless (Text.null $ Text.strip fullName) $ h1_ [class_ "card-title fs-3"] $ toHtml fullName
      when hasDisplayRules $ div_ [class_ "text-muted mb-4"] $ toHtml $ displayBadges $ NE.toList userRoles
      div_ [class_ "row"] $ do
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Email"
          let (UserEmail email) = userEmail
          p_ [class_ "fs-5"] $ a_ [href_ $ "mailto:" <> showEmail email] $ toHtml $ showEmail email
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Addresse"
          p_ [class_ "fs-5", style_ "whitespace: pre"] $ toHtml' userAddress
      div_ [class_ "row"] $ do
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Handynummer"
          p_ [class_ "fs-5"] $ toHtml' userMobilePhoneNr
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Festnetz"
          p_ [class_ "fs-5"] $ toHtml' userLandlineNr
      div_ [class_ "row"] $ do
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Geburtstag"
          p_ [class_ "fs-5"] $ toHtml' userBirthday
      div_ [class_ "row"] $ do
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Name des Partners"
          p_ [class_ "fs-5"] $ toHtml (fromMaybe "" userFirstNamePartner <> " " <> fromMaybe "" userLastNamePartner)
        div_ [class_ "mb-2 col-md-6"] $ do
          small_ [class_ "text-muted"] "Geburtstag des Partners"
          p_ [class_ "fs-5"] $ toHtml' userBirthdayPartner
    when (canDelete || canEdit) $
      div_ [class_ "card-footer d-flex d-grid"] $ do
        when canDelete $
          a_
            [ href_ . Text.pack $ "/nutzer/" <> show uid <> "/loeschen",
              class_ "link link-danger me-4"
            ]
            "Nutzer löschen"
        when canEdit $
          a_
            [ href_ . Text.pack $ "/nutzer/" <> show uid <> "/editieren",
              class_ "link link-secondary me-4"
            ]
            "Nutzer editieren"
  where
    toHtml' = toHtml . fromMaybe ""
    displayBadges = Text.intercalate ", " . map showBadge . filter dropUser
    dropUser User = False
    dropUser _ = True
    showBadge President = "Präsident"
    showBadge Passive = "Passiv"
    showBadge Board = "Vorstand"
    showBadge Admin = "Administrator"
    showBadge User = "Nutzer"

get ::
  (MonadIO m) =>
  SQLite.Connection ->
  Int ->
  Authenticated ->
  m (Maybe LayoutStub)
get conn paramId auth = do
  let userIdToShow = UserId paramId
      userIsAdmin = Session.isAdmin auth
      User.Session {..} = Session.get' auth
      isOwnProfile = sessionUserId == userIdToShow
  user <- liftIO $ getUser conn userIdToShow
  return $ case user of
    Nothing -> Nothing
    Just userProfile -> do
      Just . LayoutStub "Nutzerprofil" (Just Profile) $
        div_
          [class_ "container p-3"]
          ( render
              userProfile
              (CanDelete userIsAdmin)
              (CanEdit (isOwnProfile || userIsAdmin))
          )
