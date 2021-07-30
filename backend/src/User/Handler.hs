module User.Handler
  ( createGet,
    createPost,
    deletePost,
    deleteGet,
    editPost,
    editGet,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (i)
import qualified Database.SQLite.Simple as SQLite
import qualified Katip as K
import Layout (LayoutStub (..))
import Lucid
import qualified Network.Wai as Wai
import qualified Session.Auth as Auth
import qualified UnliftIO
import qualified User.Email as UserEmail
import User.Form (CanEditRoles (..), FormInput (..), emptyForm, makeProfile, render)
import qualified User.Id as User
import qualified User.Role.DB as User.Role
import User.Role.Role (Role (..))
import qualified User.User as User
import Wai (parseParams)

createGet ::
  (MonadIO m) =>
  Auth.Admin ->
  m LayoutStub
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

createPost ::
  ( MonadIO m,
    MonadReader env m,
    K.KatipContext m,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env,
    MonadThrow m
  ) =>
  SQLite.Connection ->
  Wai.Request ->
  Auth.Admin ->
  m LayoutStub
createPost conn req _ = do
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
          (paramb "inputIsPassive")
          (paramt "inputAddress")
          (paramt "inputFirstName")
          (paramt "inputFirstNamePartner")
          (paramt "inputLastName")
          (paramt "inputLastNamePartner")
          (paramt "inputMobile")
          (paramt "inputLandline")
  (liftIO $ makeProfile input) >>= \case
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
    Right (profile@User.Profile {..}) -> do
      UnliftIO.withRunInIO $ \runInIO ->
        SQLite.withTransaction
          conn
          ( runInIO $ do
              User.save profile
              (userid :: Int) <- liftIO $ fromIntegral <$> SQLite.lastInsertRowId conn
              User.Role.save (User.Id userid) (NE.toList userRoles)
          )
      let (UserEmail.Email email) = userEmail
      return $
        LayoutStub "Nutzer Hinzufügen" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> UserEmail.show email <> " erfolgreich erstellt"

deletePost ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  User.Id ->
  Auth.Admin ->
  m LayoutStub
deletePost userId _ = do
  User.get userId >>= \case
    Nothing -> throwString $ "edit user but no user found for id: " <> show userId
    Just (_, userProfile) -> do
      User.delete userId
      return $
        LayoutStub "Nutzerprofil" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> show (User.userEmail userProfile) <> " erfolgreich gelöscht"

deleteGet ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  User.Id ->
  Auth.Admin ->
  m LayoutStub
deleteGet userId@(User.Id uid) _ = do
  User.get userId >>= \case
    Nothing -> throwString $ "delete user but no user for eid found: " <> show userId
    Just (_, userProfile) -> do
      return . LayoutStub "Nutzerprofil" Nothing $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-danger mb-4", role_ "alert"] $
              toHtml ("Nutzer " <> show (User.userEmail userProfile) <> " wirklich löschen?")
            form_
              [ action_ [i|/nutzer/#{uid}/loeschen|],
                method_ "post",
                class_ "d-flex justify-content-center"
              ]
              $ button_ [class_ "btn btn-primary", type_ "submit"] "Ja, Nutzer löschen!"

editGet ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  User.Id ->
  Auth.Authenticated ->
  m LayoutStub
editGet userIdToEdit@(User.Id uid) auth = do
  user <- User.get userIdToEdit
  return $ case user of
    Nothing -> LayoutStub "Fehler" Nothing $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just (_, User.Profile {..}) ->
      let (UserEmail.Email email) = userEmail
       in LayoutStub "Nutzer Editieren" Nothing $
            div_ [class_ "container p-3 d-flex justify-content-center"] $
              User.Form.render
                (CanEditRoles $ Auth.isAdmin' auth)
                "Nutzer editieren"
                [i|/nutzer/#{uid}/editieren|]
                ( FormInput
                    { inputEmail = UserEmail.show email,
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

editPost ::
  ( MonadIO m,
    MonadThrow m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Wai.Request ->
  User.Id ->
  Auth.Authenticated ->
  m LayoutStub
editPost req userId auth = do
  rolesForUserToUpdate <- User.Role.get userId
  params <- liftIO $ parseParams req
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
  (liftIO $ makeProfile input) >>= \case
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
    Right profile -> do
      User.update userId profile
      let (UserEmail.Email email) = User.userEmail profile
      return $
        LayoutStub "Nutzer Editieren" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> UserEmail.show email <> " erfolgreich editiert"
  where
    userIsAdmin = Auth.isAdmin' auth
