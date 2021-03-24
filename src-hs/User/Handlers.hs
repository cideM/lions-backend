{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module User.Handlers
  ( showProfile,
    showDeleteConfirmation,
    deleteUser,
    showAddUserForm,
    showEditUserForm,
    updateExistingUser,
    saveNewUser,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Debug.Trace
import Layout (layout)
import Locale (german)
import Lucid
import qualified System.Log.FastLogger as Log
import TextShow
import User.DB (deleteUserById, getRolesFromDb, getUser, saveUser, saveUserRoles, updateUser)
import User.Domain (Role (..), UserId (..), UserProfile (..), isAdmin, isBoard, isPresident, showEmail)
import User.Form (CanEditRoles (..), FormInput (..), emptyForm, makeProfile, render)
import User.Profile (CanDelete (..), CanEdit (..))
import qualified User.Profile
import qualified Web.Scotty as S

showAddUserForm ::
  S.ActionM ()
showAddUserForm =
  S.html . renderText $
    layout "Nutzer Hinzufügen" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        render
          (CanEditRoles True)
          "Nutzer erstellen"
          "/nutzer/neu"
          (FormInput "" "" "" False False False "" "" "" "" "" "" "")
          emptyForm

renderDateForInput :: Time.Day -> Text
renderDateForInput = Text.pack . Time.formatTime german "%d.%m.%Y"

showEditUserForm ::
  SQLite.Connection ->
  [Role] ->
  UserId ->
  S.ActionM ()
showEditUserForm conn sessionRoles userid@(UserId uid) = do
  user <- S.liftAndCatchIO $ getUser conn userid
  S.html . renderText $ case user of
    Nothing -> layout "Fehler" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just (roles, (_, UserProfile {..})) ->
      layout "Nutzer Editieren" Nothing $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          render
            (CanEditRoles $ any isAdmin sessionRoles)
            "Nutzer editieren"
            ("/nutzer/" <> Text.pack (show uid) <> "/editieren")
            ( FormInput
                { inputEmail = showEmail userEmail,
                  inputBirthday = maybe "" renderDateForInput userBirthday,
                  inputBirthdayPartner = maybe "" renderDateForInput userBirthdayPartner,
                  inputIsAdmin = any isAdmin roles,
                  inputIsBoard = any isBoard roles,
                  inputIsPresident = any isPresident roles,
                  inputAddress = fromMaybe "" userAddress,
                  inputFirstName = fromMaybe "" userFirstName,
                  inputFirstNamePartner = fromMaybe "" userFirstNamePartner,
                  inputLastName = fromMaybe "" userLastName,
                  inputMobile = fromMaybe "" userMobilePhoneNr,
                  inputLandline = fromMaybe "" userLandlineNr,
                  inputLastNamePartner = fromMaybe "" userLastNamePartner
                }
            )
            emptyForm

updateExistingUser ::
  [Role] ->
  Log.FastLogger ->
  SQLite.Connection ->
  S.ActionM ()
updateExistingUser sessionRoles logger conn = do
  let paramt name = S.param (traceShowId name) `S.rescue` const (return "")
      paramb name = (True <$ (S.param name :: S.ActionM Text)) `S.rescue` const (return False)
  (i :: Int) <- S.param "id" `S.rescue` const (S.raise "no user id found for updating user")
  let userId = UserId i
      loggedInAsAdmin = any isAdmin sessionRoles
  rolesForUserToUpdate <- S.liftAndCatchIO $ getRolesFromDb conn userId
  let isAdminNow = maybe False (any isAdmin) rolesForUserToUpdate
      isBoardNow = maybe False (any isBoard) rolesForUserToUpdate
      isPresidentNow = maybe False (any isPresident) rolesForUserToUpdate
  input <-
    FormInput <$> paramt "inputEmail"
      <*> paramt "inputBirthday"
      <*> paramt "inputBirthdayPartner"
      <*> (if loggedInAsAdmin then paramb "inputIsAdmin" else pure isAdminNow)
      <*> (if loggedInAsAdmin then paramb "inputIsBoard" else pure isBoardNow)
      <*> (if loggedInAsAdmin then paramb "inputIsPresident" else pure isPresidentNow)
      <*> paramt "inputAddress"
      <*> paramt "inputFirstName"
      <*> paramt "inputFirstNamePartner"
      <*> paramt "inputLastName"
      <*> paramt "inputLastNamePartner"
      <*> paramt "inputMobile"
      <*> paramt "inputLandline"
  case makeProfile (traceShowId input) of
    Left state ->
      S.html . renderText $
        layout "Nutzer Editieren" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            render
              (CanEditRoles $ any isAdmin sessionRoles)
              "Nutzer editieren"
              "/nutzer/neu"
              input
              state
    Right (profile, roles) -> do
      S.liftAndCatchIO $ updateUser conn userId roles profile
      S.liftAndCatchIO . logger . Log.toLogStr $ "editing user: " <> show profile <> "\n"
      S.liftAndCatchIO . logger . Log.toLogStr $ "editing user roles: " <> show roles <> "\n"
      S.html . renderText $
        layout "Nutzer Editieren" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> show (userEmail profile) <> " erfolgreich editiert"

saveNewUser ::
  Log.FastLogger ->
  SQLite.Connection ->
  S.ActionM ()
saveNewUser logger conn = do
  let paramt name = S.param (traceShowId name) `S.rescue` const (return "")
      paramb name = (True <$ (S.param name :: S.ActionM Text)) `S.rescue` const (return False)
  input <-
    FormInput <$> paramt "inputEmail"
      <*> paramt "inputBirthday"
      <*> paramt "inputBirthdayPartner"
      <*> paramb "inputIsAdmin"
      <*> paramb "inputIsBoard"
      <*> paramb "inputIsPresident"
      <*> paramt "inputAddress"
      <*> paramt "inputFirstName"
      <*> paramt "inputFirstNamePartner"
      <*> paramt "inputLastName"
      <*> paramt "inputLastNamePartner"
      <*> paramt "inputMobile"
      <*> paramt "inputLandline"
  case makeProfile (traceShowId input) of
    Left state ->
      S.html . renderText $
        layout "Nutzer Hinzufügen" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            render
              (CanEditRoles True)
              "Nutzer erstellen"
              "/nutzer/neu"
              input
              state
    Right (profile, roles) -> do
      S.liftAndCatchIO $
        SQLite.withTransaction
          conn
          $ do
            saveUser conn profile
            (userid :: Int) <- fromIntegral <$> SQLite.lastInsertRowId conn
            saveUserRoles conn (UserId (traceShow ("userid " <> show userid) userid)) roles
      S.liftAndCatchIO . logger . Log.toLogStr $ "adding user: " <> show profile <> "\n"
      S.liftAndCatchIO . logger . Log.toLogStr $ "adding user roles: " <> show roles <> "\n"
      S.html . renderText $
        layout "Nutzer Hinzufügen" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> show (userEmail profile) <> " erfolgreich erstellt"

showProfile ::
  SQLite.Connection ->
  [Role] ->
  UserId ->
  S.ActionM ()
showProfile conn roles loggedInUserId = do
  (i :: Int) <- S.param "id"
  let userIsAdmin = any isAdmin roles
      userIdToShow = UserId i
      isOwnProfile = loggedInUserId == userIdToShow
  user <- S.liftAndCatchIO $ getUser conn userIdToShow
  S.html . renderText $ case user of
    Nothing -> layout "Fehler" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just (userRoles, (otherUserId, userProfile)) -> do
      layout "Nutzerprofil" Nothing $
        div_
          [class_ "container p-3"]
          ( User.Profile.render
              userRoles
              otherUserId
              userProfile
              (CanDelete userIsAdmin)
              (CanEdit (isOwnProfile || userIsAdmin))
          )

deleteUser ::
  SQLite.Connection ->
  S.ActionM ()
deleteUser conn = do
  (i :: Int) <- S.param "id"
  let userId = UserId i
  user <- S.liftAndCatchIO $ getUser conn userId
  case user of
    Nothing -> S.html . renderText . layout "Fehler" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just (_, (_, userProfile)) -> do
      S.liftAndCatchIO $ deleteUserById conn userId
      S.html . renderText $
        layout "Nutzerprofil" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> show (userEmail userProfile) <> " erfolgreich gelöscht"

showDeleteConfirmation ::
  SQLite.Connection ->
  S.ActionM ()
showDeleteConfirmation conn = do
  (i :: Int) <- S.param "id"
  let userId = UserId i
  user <- S.liftAndCatchIO $ getUser conn userId
  S.html . renderText $ case user of
    Nothing -> layout "Fehler" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just (_, (_, userProfile)) -> do
      layout "Nutzerprofil" Nothing $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-danger mb-4", role_ "alert"] $
              toHtml ("Nutzer " <> show (userEmail userProfile) <> " wirklich löschen?")
            form_
              [ action_ $ "/nutzer/" <> (showt $ userId) <> "/löschen",
                method_ "post",
                class_ "d-flex justify-content-center"
              ]
              $ button_ [class_ "btn btn-primary", type_ "submit"] "Ja, Nutzer löschen!"
