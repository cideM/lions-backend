{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Capability.Reader (HasReader (..), ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Katip
import Layout (layout)
import Locale (german)
import Lucid
import qualified Network.Wai as Wai
import qualified Routes.Data as Auth
import TextShow
import User.DB (deleteUserById, getRolesFromDb, getUser, saveUser, saveUserRoles, updateUser)
import User.Domain (UserEmail (..), UserId (..), UserProfile (..), isAdmin, isBoard, isPresident, showEmail)
import User.Form (CanEditRoles (..), FormInput (..), emptyForm, makeProfile, render)
import User.Profile (CanDelete (..), CanEdit (..))
import qualified User.Profile
import Wai (parseParams)

showAddUserForm ::
  Monad m =>
  Auth.AdminUser ->
  m (Html ())
showAddUserForm _ =
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
  ( MonadIO m,
    HasReader "dbConn" SQLite.Connection m
  ) =>
  UserId ->
  Auth.Authenticated ->
  m (Html ())
showEditUserForm userId@(UserId i) auth = do
  let Auth.UserSession _ sessionRoles = case auth of
        Auth.IsUser session -> session
        Auth.IsAdmin (Auth.AdminUser session) -> session
  conn <- ask @"dbConn"
  user <- liftIO $ getUser conn userId
  return $ case user of
    Nothing -> layout "Fehler" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just (roles, (_, UserProfile {..})) ->
      let (UserEmail email) = userEmail
       in layout "Nutzer Editieren" Nothing $
            div_ [class_ "container p-3 d-flex justify-content-center"] $
              render
                (CanEditRoles $ any isAdmin sessionRoles)
                "Nutzer editieren"
                ("/nutzer/" <> Text.pack (show i) <> "/editieren")
                ( FormInput
                    { inputEmail = showEmail email,
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
  ( MonadIO m,
    KatipContext m,
    HasReader "dbConn" SQLite.Connection m
  ) =>
  Wai.Request ->
  UserId ->
  Auth.Authenticated ->
  m (Html ())
updateExistingUser req userId auth = do
  conn <- ask @"dbConn"
  rolesForUserToUpdate <- liftIO $ getRolesFromDb conn userId
  params <- liftIO $ parseParams req
  let paramt name = Map.findWithDefault "" name params
      paramb name = isJust $ Map.lookup name params
      loggedInAsAdmin = any isAdmin sessionRoles
      isAdminNow = maybe False (any isAdmin) rolesForUserToUpdate
      isBoardNow = maybe False (any isBoard) rolesForUserToUpdate
      isPresidentNow = maybe False (any isPresident) rolesForUserToUpdate
      Auth.UserSession _ sessionRoles = case auth of
        Auth.IsUser session -> session
        Auth.IsAdmin (Auth.AdminUser session) -> session
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
  case makeProfile input of
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
      liftIO $ updateUser conn userId roles profile
      katipAddContext (sl "roles" roles) $
        katipAddContext (sl "profile" profile) $ do
          logLocM DebugS "editing user"
          return $
            layout "Nutzer Editieren" Nothing $
              div_ [class_ "container p-3 d-flex justify-content-center"] $
                div_ [class_ "row col-6"] $ do
                  p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                    "Nutzer " <> show (userEmail profile) <> " erfolgreich editiert"

-- TODO: Duplication
saveNewUser ::
  ( MonadIO m,
    HasReader "dbConn" SQLite.Connection m,
    KatipContext m
  ) =>
  Wai.Request ->
  Auth.AdminUser ->
  m (Html ())
saveNewUser req _ = do
  conn <- ask @"dbConn"
  params <- liftIO $ parseParams req
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
  case makeProfile input of
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
      liftIO $
        SQLite.withTransaction
          conn
          $ do
            saveUser conn profile
            (userid :: Int) <- fromIntegral <$> SQLite.lastInsertRowId conn
            saveUserRoles conn (UserId userid) roles
      katipAddContext (sl "roles" roles) $
        katipAddContext (sl "profile" profile) $ do
          logLocM DebugS "adding user"
          return $
            layout "Nutzer Hinzufügen" Nothing $
              div_ [class_ "container p-3 d-flex justify-content-center"] $
                div_ [class_ "row col-6"] $ do
                  p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                    "Nutzer " <> show (userEmail profile) <> " erfolgreich erstellt"

showProfile ::
  ( MonadIO m,
    HasReader "dbConn" SQLite.Connection m
  ) =>
  Int ->
  Auth.Authenticated ->
  m (Html ())
showProfile paramId auth = do
  conn <- ask @"dbConn"
  let userIdToShow = UserId paramId
      userIsAdmin = case auth of
        Auth.IsAdmin _ -> True
        _ -> False
      Auth.UserSession loggedInUserId _ = case auth of
        Auth.IsUser session -> session
        Auth.IsAdmin (Auth.AdminUser session) -> session
      isOwnProfile = loggedInUserId == userIdToShow
  user <- liftIO $ getUser conn userIdToShow
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
  ( MonadIO m,
    HasReader "dbConn" SQLite.Connection m
  ) =>
  UserId ->
  Auth.AdminUser ->
  m (Html ())
deleteUser userId _ = do
  conn <- ask @"dbConn"
  user <- liftIO $ getUser conn userId
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
  ( MonadIO m,
    HasReader "dbConn" SQLite.Connection m
  ) =>
  UserId ->
  Auth.AdminUser ->
  m (Html ())
showDeleteConfirmation userId _ = do
  conn <- ask @"dbConn"
  user <- liftIO $ getUser conn userId
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
