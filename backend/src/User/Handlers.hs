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

import Control.Exception.Safe
import Control.Monad (when)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import Layout (ActiveNavLink (..), LayoutStub (..))
import Lucid
import qualified Network.Wai as Wai
import qualified Routes.Data as Auth
import User.DB (deleteUserById, getRolesFromDb, getUser, getUsers, saveUser, saveUserRoles, updateUser)
import User.Types
  ( Role (..),
    UserEmail (..),
    UserId (..),
    UserProfile (..),
    UserProfileCreate (..),
    isAdmin,
    isBoard,
    isPassive,
    isPresident,
    showEmail,
  )
import User.Form (CanEditRoles (..), FormInput (..), emptyForm, makeProfile, render)
import User.Memberlist (UserGroupToShow (..))
import qualified User.Memberlist as Memberlist
import User.Profile (CanDelete (..), CanEdit (..))
import qualified User.Profile
import Wai (parseParams, parseQueryParams)

showAddUserForm :: Auth.AdminUser -> IO LayoutStub
showAddUserForm _ =
  return $
    LayoutStub "Nutzer Hinzufügen" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        render
          (CanEditRoles True)
          "Nutzer erstellen"
          "/nutzer/neu"
          (FormInput "" "" "" False False False False "" "" "" "" "" "" "")
          emptyForm

showEditUserForm :: SQLite.Connection -> UserId -> Auth.Authenticated -> IO LayoutStub
showEditUserForm conn userIdToEdit@(UserId uid) auth = do
  let Auth.UserSession _ sessionRoles = case auth of
        Auth.IsUser session -> session
        Auth.IsAdmin (Auth.AdminUser session) -> session
  user <- getUser conn userIdToEdit
  return $ case user of
    Nothing -> LayoutStub "Fehler" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just UserProfile {..} ->
      let (UserEmail email) = userEmail
       in LayoutStub "Nutzer Editieren" Nothing $
            div_ [class_ "container p-3 d-flex justify-content-center"] $
              render
                (CanEditRoles $ any isAdmin sessionRoles)
                "Nutzer editieren"
                [i|/nutzer/#{uid}/editieren|]
                ( FormInput
                    { inputEmail = showEmail email,
                      inputBirthday = fromMaybe "" userBirthday,
                      inputBirthdayPartner = fromMaybe "" userBirthdayPartner,
                      inputIsAdmin = any isAdmin userRoles,
                      inputIsBoard = any isBoard userRoles,
                      inputIsPresident = any isPresident userRoles,
                      inputIsPassive = any isPassive userRoles,
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

updateExistingUser :: SQLite.Connection -> Wai.Request -> UserId -> Auth.Authenticated -> IO LayoutStub
updateExistingUser conn req userId auth = do
  rolesForUserToUpdate <- getRolesFromDb conn userId
  params <- parseParams req
  let paramt name = Map.findWithDefault "" name params
      paramb name = isJust $ Map.lookup name params
      loggedInAsAdmin = any isAdmin sessionRoles
      isAdminNow = maybe False (any isAdmin) rolesForUserToUpdate
      isBoardNow = maybe False (any isBoard) rolesForUserToUpdate
      isPassiveNow = maybe False (any isPassive) rolesForUserToUpdate
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
          (if loggedInAsAdmin then paramb "inputIsPassive" else isPassiveNow)
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
        LayoutStub "Nutzer Editieren" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            render
              (CanEditRoles $ any isAdmin sessionRoles)
              "Nutzer editieren"
              "/nutzer/neu"
              input
              state
    Right UserProfileCreate {..} -> do
      let profile =
            UserProfile
              { userEmail = userCreateEmail,
                userFirstName = userCreateFirstName,
                userLastName = userCreateLastName,
                userAddress = userCreateAddress,
                userMobilePhoneNr = userCreateMobilePhoneNr,
                userLandlineNr = userCreateLandlineNr,
                userBirthday = userCreateBirthday,
                userFirstNamePartner = userCreateFirstNamePartner,
                userLastNamePartner = userCreateLastNamePartner,
                userBirthdayPartner = userCreateBirthdayPartner,
                userId = userId,
                userRoles = userCreateRoles
              }
      updateUser conn userId profile
      let (UserEmail email) = userCreateEmail
      return $
        LayoutStub "Nutzer Editieren" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> showEmail email <> " erfolgreich editiert"

-- TODO: Duplication
saveNewUser :: SQLite.Connection -> Wai.Request -> Auth.AdminUser -> IO LayoutStub
saveNewUser conn req _ = do
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
          (paramb "inputIsPassive")
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
        LayoutStub "Nutzer Hinzufügen" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            render
              (CanEditRoles True)
              "Nutzer erstellen"
              "/nutzer/neu"
              input
              state
    Right (profile@UserProfileCreate {..}) -> do
      SQLite.withTransaction
        conn
        $ do
          saveUser conn profile
          (userid :: Int) <- fromIntegral <$> SQLite.lastInsertRowId conn
          saveUserRoles conn (UserId userid) (NE.toList userCreateRoles)
      let (UserEmail email) = userCreateEmail
      return $
        LayoutStub "Nutzer Hinzufügen" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> showEmail email <> " erfolgreich erstellt"

showProfile :: SQLite.Connection -> Int -> Auth.Authenticated -> IO (Maybe LayoutStub)
showProfile conn paramId auth = do
  let userIdToShow = UserId paramId
      userIsAdmin = case auth of
        Auth.IsAdmin _ -> True
        _ -> False
      Auth.UserSession loggedInUserId _ = case auth of
        Auth.IsUser session -> session
        Auth.IsAdmin (Auth.AdminUser session) -> session
      isOwnProfile = loggedInUserId == userIdToShow
  user <- getUser conn userIdToShow
  return $ case user of
    Nothing -> Nothing
    Just userProfile -> do
      Just . LayoutStub "Nutzerprofil" (Just Profile) $
        div_
          [class_ "container p-3"]
          ( User.Profile.render
              userProfile
              (CanDelete userIsAdmin)
              (CanEdit (isOwnProfile || userIsAdmin))
          )

deleteUser :: SQLite.Connection -> UserId -> Auth.AdminUser -> IO LayoutStub
deleteUser conn userId _ = do
  user <- getUser conn userId
  case user of
    Nothing -> throwString $ "edit user but no user found for id: " <> show userId
    Just userProfile -> do
      deleteUserById conn userId
      return $
        LayoutStub "Nutzerprofil" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> show (userEmail userProfile) <> " erfolgreich gelöscht"

showDeleteConfirmation :: SQLite.Connection -> UserId -> Auth.AdminUser -> IO LayoutStub
showDeleteConfirmation conn userId@(UserId uid) _ = do
  user <- getUser conn userId
  case user of
    Nothing -> throwString $ "delete user but no user for eid found: " <> show userId
    Just userProfile -> do
      return . LayoutStub "Nutzerprofil" Nothing $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-danger mb-4", role_ "alert"] $
              toHtml ("Nutzer " <> show (userEmail userProfile) <> " wirklich löschen?")
            form_
              [ action_ [i|/nutzer/#{uid}/loeschen|],
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

showMemberList :: SQLite.Connection -> Wai.Request -> Auth.Authenticated -> IO LayoutStub
showMemberList conn req auth = do
  let userIsAdmin = case auth of
        Auth.IsAdmin _ -> True
        _ -> False
  let selectionRaw = Map.findWithDefault "all" "userselect" $ parseQueryParams req
  selectionParsed <-
    case parseSelection selectionRaw of
      Left e -> throwString . Text.unpack $ "invalid group selection: " <> selectionRaw <> " " <> e
      Right (v :: UserGroupToShow) -> pure v
  users <- getUsers conn
  let usersToShow = case selectionParsed of
        All -> users
        Some role -> filterUsers role users
  return $
    LayoutStub "Mitglieder" (Just Members) $
      div_ [class_ "container p-2"] $ do
        when userIsAdmin $ a_ [class_ "btn btn-primary mb-3", href_ "/nutzer/neu"] "Neues Mitglied hinzufügen"
        h1_ [class_ "h4 mb-5"] "Mitgliederliste"
        div_ [class_ "row row-cols-1 g-2"] $
          Memberlist.render usersToShow selectionParsed
  where
    filterUsers keep = filter (elem keep . userRoles)
