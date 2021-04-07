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

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Debug.Trace
import Layout (layout)
import Locale (german)
import Lucid
import qualified Network.Wai as Wai
import qualified System.Log.FastLogger as Log
import TextShow
import User.DB (deleteUserById, getRolesFromDb, getUser, saveUser, saveUserRoles, updateUser)
import User.Domain (Role (..), UserId (..), UserProfile (..), isAdmin, isBoard, isPresident, showEmail)
import User.Form (CanEditRoles (..), FormInput (..), emptyForm, makeProfile, render)
import User.Profile (CanDelete (..), CanEdit (..))
import qualified User.Profile
import Wai (parseParams)

showAddUserForm ::
  IO (Html ())
showAddUserForm =
  return $
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
  IO (Html ())
showEditUserForm conn sessionRoles userId@(UserId i) = do
  user <- getUser conn userId
  return $ case user of
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
            ("/nutzer/" <> Text.pack (show i) <> "/editieren")
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
  UserId ->
  Wai.Request ->
  IO (Html ())
updateExistingUser sessionRoles logger conn userId req = do
  rolesForUserToUpdate <- getRolesFromDb conn userId
  params <- parseParams req
  let paramt name = Map.findWithDefault "" name params
      paramb name = isJust $ Map.lookup name params
      loggedInAsAdmin = any isAdmin sessionRoles
      isAdminNow = maybe False (any isAdmin) rolesForUserToUpdate
      isBoardNow = maybe False (any isBoard) rolesForUserToUpdate
      isPresidentNow = maybe False (any isPresident) rolesForUserToUpdate
      input =
        FormInput
          (paramt "inputEmail")
          (paramt "inputBirthday")
          (paramt "inputBirthdayPartner")
          ((if loggedInAsAdmin then paramb "inputIsAdmin" else isAdminNow))
          ((if loggedInAsAdmin then paramb "inputIsBoard" else isBoardNow))
          ((if loggedInAsAdmin then paramb "inputIsPresident" else isPresidentNow))
          (paramt "inputAddress")
          (paramt "inputFirstName")
          (paramt "inputFirstNamePartner")
          (paramt "inputLastName")
          (paramt "inputLastNamePartner")
          (paramt "inputMobile")
          (paramt "inputLandline")
  case makeProfile (traceShowId input) of
    Left state ->
      return $
        layout "Nutzer Editieren" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            render
              (CanEditRoles $ any isAdmin sessionRoles)
              "Nutzer editieren"
              "/nutzer/neu"
              input
              state
    Right (profile, roles) -> do
      updateUser conn userId roles profile
      logger . Log.toLogStr $ "editing user: " <> show profile <> "\n"
      logger . Log.toLogStr $ "editing user roles: " <> show roles <> "\n"
      return $
        layout "Nutzer Editieren" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> show (userEmail profile) <> " erfolgreich editiert"

-- TODO: Duplication
saveNewUser ::
  Log.FastLogger ->
  SQLite.Connection ->
  Wai.Request ->
  IO (Html ())
saveNewUser logger conn req = do
  params <- parseParams req
  let paramt name = Map.findWithDefault "" name params
      paramb name = isJust $ Map.lookup name params
      input =
        FormInput
          (paramt "inputEmail")
          (paramt "inputBirthday")
          (paramt "inputBirthdayPartner")
          (paramb "inputIsAdmin")
          (paramb "inputIsBoard")
          (paramb "inputIsPresident")
          (paramt "inputAddress")
          (paramt "inputFirstName")
          (paramt "inputFirstNamePartner")
          (paramt "inputLastName")
          (paramt "inputLastNamePartner")
          (paramt "inputMobile")
          (paramt "inputLandline")
  case makeProfile (traceShowId input) of
    Left state ->
      return $
        layout "Nutzer Hinzufügen" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            render
              (CanEditRoles True)
              "Nutzer erstellen"
              "/nutzer/neu"
              input
              state
    Right (profile, roles) -> do
      SQLite.withTransaction
        conn
        $ do
          saveUser conn profile
          (userid :: Int) <- fromIntegral <$> SQLite.lastInsertRowId conn
          saveUserRoles conn (UserId (traceShow ("userid " <> show userid) userid)) roles
      logger . Log.toLogStr $ "adding user: " <> show profile <> "\n"
      logger . Log.toLogStr $ "adding user roles: " <> show roles <> "\n"
      return $
        layout "Nutzer Hinzufügen" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> show (userEmail profile) <> " erfolgreich erstellt"

showProfile ::
  SQLite.Connection ->
  [Role] ->
  Int ->
  UserId ->
  Wai.Request ->
  IO (Html ())
showProfile conn roles paramId loggedInUserId _ = do
  let userIdToShow = UserId paramId
      userIsAdmin = any isAdmin roles
      isOwnProfile = loggedInUserId == userIdToShow
  user <- getUser conn userIdToShow
  return $ case user of
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
  UserId ->
  IO (Html ())
deleteUser conn userId = do
  user <- getUser conn userId
  case user of
    Nothing -> return . layout "Fehler" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just (_, (_, userProfile)) -> do
      deleteUserById conn userId
      return $
        layout "Nutzerprofil" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> show (userEmail userProfile) <> " erfolgreich gelöscht"

showDeleteConfirmation ::
  SQLite.Connection ->
  UserId ->
  IO (Html ())
showDeleteConfirmation conn userId = do
  user <- getUser conn userId
  return $ case user of
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
              [ action_ $ "/nutzer/" <> showt userId <> "/löschen",
                method_ "post",
                class_ "d-flex justify-content-center"
              ]
              $ button_ [class_ "btn btn-primary", type_ "submit"] "Ja, Nutzer löschen!"
