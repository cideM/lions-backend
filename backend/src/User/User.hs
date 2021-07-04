module User.User
  ( createGet,
    createPost,
    deletePost,
    deleteGet,
    editPost,
    editGet,
  )
where

import Control.Exception.Safe
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (i)
import qualified Database.SQLite.Simple as SQLite
import Layout (LayoutStub (..))
import Lucid
import qualified Network.Wai as Wai
import qualified Session as Session
import User.DB (deleteUserById, getUser, saveUser, saveUserRoles, getRolesFromDb, updateUser)
import User.Form (CanEditRoles (..), FormInput (..), emptyForm, makeProfile, render)
import User.Types
  ( Role (..),
    UserEmail (..),
    UserId (..),
    UserProfile (..),
    UserProfileCreate (..),
    showEmail,
  )
import Wai (parseParams)

createGet :: Session.AdminUser -> IO LayoutStub
createGet _ =
  return $
    LayoutStub "Nutzer Hinzufügen" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        User.Form.render
          (CanEditRoles True)
          "Nutzer erstellen"
          "/nutzer/neu"
          (FormInput "" "" "" False False False False "" "" "" "" "" "" "")
          emptyForm

createPost :: SQLite.Connection -> Wai.Request -> Session.AdminUser -> IO LayoutStub
createPost conn req _ = do
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
            User.Form.render
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

deletePost :: SQLite.Connection -> UserId -> Session.AdminUser -> IO LayoutStub
deletePost conn userId _ = do
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

deleteGet :: SQLite.Connection -> UserId -> Session.AdminUser -> IO LayoutStub
deleteGet conn userId@(UserId uid) _ = do
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

editGet :: SQLite.Connection -> UserId -> Session.Authenticated -> IO LayoutStub
editGet conn userIdToEdit@(UserId uid) auth = do
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
              User.Form.render
                (CanEditRoles userIsAdmin)
                "Nutzer editieren"
                [i|/nutzer/#{uid}/editieren|]
                ( FormInput
                    { inputEmail = showEmail email,
                      inputBirthday = fromMaybe "" userBirthday,
                      inputBirthdayPartner = fromMaybe "" userBirthdayPartner,
                      inputIsAdmin = any ((==) Admin) userRoles,
                      inputIsBoard = any ((==) Board) userRoles,
                      inputIsPresident = any ((==) President) userRoles,
                      inputIsPassive = any ((==) Passive) userRoles,
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
  where
    userIsAdmin = case auth of
      Session.IsAdmin _ -> True
      _ -> False

editPost :: SQLite.Connection -> Wai.Request -> UserId -> Session.Authenticated -> IO LayoutStub
editPost conn req userId auth = do
  rolesForUserToUpdate <- getRolesFromDb conn userId
  params <- parseParams req
  let paramt name = Map.findWithDefault "" name params
      paramb name = isJust $ Map.lookup name params
      isRole role = maybe False (any ((==) role)) rolesForUserToUpdate
      input =
        FormInput
          (paramt "inputEmail")
          (paramt "inputBirthday")
          (paramt "inputBirthdayPartner")
          (if userIsAdmin then paramb "inputIsAdmin" else isRole Admin)
          (if userIsAdmin then paramb "inputIsBoard" else isRole Board)
          (if userIsAdmin then paramb "inputIsPresident" else isRole President)
          (if userIsAdmin then paramb "inputIsPassive" else isRole Passive)
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
            User.Form.render
              (CanEditRoles userIsAdmin)
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
  where
    userIsAdmin = case auth of
      Session.IsAdmin _ -> True
      _ -> False