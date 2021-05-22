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
    showMemberList,
    deleteUser,
    showAddUserForm,
    showEditUserForm,
    updateExistingUser,
    saveNewUser,
  )
where

import Capability.Reader (HasReader (..), ask)
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List.NonEmpty as NE
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Debug.Trace
import Katip
import Layout (ActiveNavLink (..), layout)
import Locale (german)
import Lucid
import qualified Network.Wai as Wai
import qualified Routes.Data as Auth
import User.DB (deleteUserById, getRolesFromDb, getUser, getUsers, saveUser, saveUserRoles, updateUser)
import User.Domain
  ( Role (..),
    UserEmail (..),
    UserId (..),
    UserProfile (..),
    UserProfileCreate (..),
    isAdmin,
    isBoard,
    isPresident,
    showEmail,
  )
import User.Form (CanEditRoles (..), FormInput (..), emptyForm, makeProfile, render)
import User.Memberlist (UserGroupToShow (..))
import qualified User.Memberlist as Memberlist
import User.Profile (CanDelete (..), CanEdit (..))
import qualified User.Profile
import Wai (parseParams, parseQueryParams)

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
showEditUserForm userIdToEdit@(UserId i) auth = do
  let Auth.UserSession _ sessionRoles = case auth of
        Auth.IsUser session -> session
        Auth.IsAdmin (Auth.AdminUser session) -> session
  conn <- ask @"dbConn"
  user <- liftIO $ getUser conn userIdToEdit
  return $ case user of
    Nothing -> layout "Fehler" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just UserProfile {..} ->
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
                      inputIsAdmin = any isAdmin userRoles,
                      inputIsBoard = any isBoard userRoles,
                      inputIsPresident = any isPresident userRoles,
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
    MonadThrow m,
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
          (if loggedInAsAdmin then paramb "inputIsAdmin" else isAdminNow)
          (if loggedInAsAdmin then paramb "inputIsBoard" else isBoardNow)
          (if loggedInAsAdmin then paramb "inputIsPresident" else isPresidentNow)
          (paramt "inputAddress")
          (paramt "inputFirstName")
          (paramt "inputFirstNamePartner")
          (paramt "inputLastName")
          (paramt "inputLastNamePartner")
          (paramt "inputMobile")
          (paramt "inputLandline")
  makeProfile input >>= \case
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
    Right profile@UserProfileCreate {..} -> do
      liftIO $ updateUser conn userId profile
      katipAddContext (sl "roles" userCreateRoles) $
        katipAddContext (sl "profile" profile) $ do
          logLocM DebugS "editing user"
          return $
            layout "Nutzer Editieren" Nothing $
              div_ [class_ "container p-3 d-flex justify-content-center"] $
                div_ [class_ "row col-6"] $ do
                  p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                    "Nutzer " <> show userCreateEmail <> " erfolgreich editiert"

-- TODO: Duplication
saveNewUser ::
  ( MonadIO m,
    MonadThrow m,
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
  makeProfile input >>= \case
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
    Right (profile@UserProfileCreate {..}) -> do
      liftIO $
        SQLite.withTransaction
          conn
          $ do
            saveUser conn profile
            (userid :: Int) <- fromIntegral <$> SQLite.lastInsertRowId conn
            saveUserRoles conn (UserId userid) (NE.toList userCreateRoles)
      katipAddContext (sl "roles" $ NE.toList userCreateRoles) $
        katipAddContext (sl "profile" profile) $ do
          logLocM DebugS "adding user"
          return $
            layout "Nutzer Hinzufügen" Nothing $
              div_ [class_ "container p-3 d-flex justify-content-center"] $
                div_ [class_ "row col-6"] $ do
                  p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                    "Nutzer " <> show userCreateEmail <> " erfolgreich erstellt"

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
  -- TODO: This needs to return a 404
  return $ case user of
    Nothing -> layout "Fehler" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just userProfile -> do
      layout "Nutzerprofil" Nothing $
        div_
          [class_ "container p-3"]
          ( User.Profile.render
              userProfile
              (CanDelete userIsAdmin)
              (CanEdit (isOwnProfile || userIsAdmin))
          )

deleteUser ::
  ( MonadIO m,
    MonadThrow m,
    HasReader "dbConn" SQLite.Connection m
  ) =>
  UserId ->
  Auth.AdminUser ->
  m (Html ())
deleteUser userId _ = do
  conn <- ask @"dbConn"
  user <- liftIO $ getUser conn userId
  case user of
    Nothing -> throwString $ "edit user but no user found for id: " <> show userId
    Just userProfile -> do
      deleteUserById conn userId
      return $
        layout "Nutzerprofil" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> show (userEmail userProfile) <> " erfolgreich gelöscht"

showDeleteConfirmation ::
  ( MonadIO m,
    MonadThrow m,
    HasReader "dbConn" SQLite.Connection m
  ) =>
  UserId ->
  Auth.AdminUser ->
  m (Html ())
showDeleteConfirmation userId _ = do
  conn <- ask @"dbConn"
  user <- liftIO $ getUser conn userId
  case user of
    Nothing -> throwString $ "delete user but no user for eid found: " <> show userId
    Just userProfile -> do
      return . layout "Nutzerprofil" Nothing $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-danger mb-4", role_ "alert"] $
              toHtml ("Nutzer " <> show (userEmail userProfile) <> " wirklich löschen?")
            form_
              [ action_ . Text.pack $ "/nutzer/" <> show userId <> "/loeschen",
                method_ "post",
                class_ "d-flex justify-content-center"
              ]
              $ button_ [class_ "btn btn-primary", type_ "submit"] "Ja, Nutzer löschen!"

parseSelection :: Text -> Either Text UserGroupToShow
parseSelection "all" = Right All
parseSelection "admin" = Right $ Some Admin
parseSelection "board" = Right $ Some Board
parseSelection "user" = Right $ Some User
parseSelection "president" = Right $ Some President
parseSelection v = Left $ "unknown user group: " <> v

showMemberList ::
  ( MonadIO m,
    MonadCatch m,
    HasReader "dbConn" SQLite.Connection m
  ) =>
  Wai.Request ->
  Auth.Authenticated ->
  m (Html ())
showMemberList req auth = do
  let userIsAdmin = case auth of
        Auth.IsAdmin _ -> True
        _ -> False
  conn <- ask @"dbConn"
  let selectionRaw = traceShowId $ Map.findWithDefault "all" "userselect" $ parseQueryParams req
  selectionParsed <-
    traceShowId <$> case parseSelection selectionRaw of
      Left e -> liftIO . throwString . Text.unpack $ "invalid group selection: " <> selectionRaw <> " " <> e
      Right (v :: UserGroupToShow) -> pure v
  users <- liftIO $ getUsers conn
  let usersToShow = case selectionParsed of
        All -> users
        Some role -> filterUsers role users
  return $
    layout "Mitglieder" (Just Members) $
      div_ [class_ "container p-2"] $ do
        when userIsAdmin $ button_ [class_ "btn btn-primary mb-3", href_ "/nutzer/neu"] "Neues Mitglied hinzufügen"
        div_ [class_ "row row-cols-1 g-2"] $
          Memberlist.render usersToShow selectionParsed
  where
    filterUsers keep = filter (elem keep . userRoles)
