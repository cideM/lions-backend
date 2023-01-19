module User.Handler
  ( createGet,
    createPost,
    deletePost,
    deleteGet,
    editPost,
    editGet,
    viewGet,
    viewListGet,
  )
where

import qualified App
import Control.Error
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import qualified Katip as K
import Layout (LayoutStub (..))
import Lucid
import qualified Network.Wai as Wai
import qualified UnliftIO
import User.Form (CanEditRoles (..), FormInput (..), emptyForm, makeProfile, render)
import qualified User.Id as User
import qualified User.List
import qualified User.Profile as UserProfile
import qualified User.Role.DB as User.Role
import User.Role.Role (Role (..))
import qualified User.Session
import qualified User.User as User
import Wai (parseParams, parseQueryParams)

createGet ::
  (MonadIO m) =>
  User.Session.Admin ->
  m LayoutStub
createGet _ =
  return $
    LayoutStub "Nutzer Hinzufügen" $
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
  Wai.Request ->
  User.Session.Admin ->
  m LayoutStub
createPost req _ = do
  conn <- asks App.getDb
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
        LayoutStub "Nutzer Hinzufügen" $
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
      return $
        LayoutStub "Nutzer Hinzufügen" $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> show userEmail <> " erfolgreich erstellt"

deletePost ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  User.Id ->
  User.Session.Admin ->
  m LayoutStub
deletePost userId _ = do
  User.get userId >>= \case
    Nothing -> throwString $ "edit user but no user found for id: " <> show userId
    Just (_, userProfile) -> do
      User.delete userId
      return $
        LayoutStub "Nutzerprofil" $
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
  User.Session.Admin ->
  m LayoutStub
deleteGet userId@(User.Id uid) _ = do
  User.get userId >>= \case
    Nothing -> throwString $ "delete user but no user for eid found: " <> show userId
    Just (_, userProfile) -> do
      return . LayoutStub "Nutzerprofil" $
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
  User.Session.Authenticated ->
  m LayoutStub
editGet userIdToEdit@(User.Id uid) auth = do
  user <- User.get userIdToEdit
  return $ case user of
    Nothing -> LayoutStub "Fehler" $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just (_, User.Profile {..}) ->
      LayoutStub "Nutzer Editieren" $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          User.Form.render
            (CanEditRoles $ User.Session.isAdmin' auth)
            "Nutzer editieren"
            [i|/nutzer/#{uid}/editieren|]
            ( FormInput
                { inputEmail = Text.pack $ show userEmail,
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

(?*) :: (MonadError e m) => MaybeT m b -> e -> m b
(?*) x e = runMaybeT x >>= liftEither . note e

editPost ::
  ( MonadIO m,
    MonadThrow m,
    MonadReader env m,
    App.HasDb env
  ) =>
  Wai.Request ->
  User.Id ->
  User.Session.Authenticated ->
  m LayoutStub
editPost req userId auth = do
  rolesForUserToUpdate <-
    runExceptT (User.Role.get userId ?* ("no rules found for userId" <> show userId)) >>= \case
      Left e -> throwString e
      Right v -> return v

  params <- liftIO $ parseParams req

  let paramt name = Map.findWithDefault "" name params
      paramb name = isJust $ Map.lookup name params
      isRole role = any ((==) role) rolesForUserToUpdate
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
        LayoutStub "Nutzer Editieren" $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            User.Form.render
              (CanEditRoles userIsAdmin)
              "Nutzer editieren"
              "/nutzer/neu"
              input
              state
    Right profile -> do
      User.update userId profile
      return $
        LayoutStub "Nutzer Editieren" $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Nutzer " <> (show $ User.userEmail profile) <> " erfolgreich editiert"
  where
    userIsAdmin = User.Session.isAdmin' auth

viewGet ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Int ->
  User.Session.Authenticated ->
  m (Maybe LayoutStub)
viewGet paramId auth = do
  let userIdToShow = User.Id paramId
      userIsAdmin = User.Session.isAdmin' auth
      User.Session.Session {..} = User.Session.get' auth
      isOwnProfile = sessionUserId == userIdToShow
  user <- User.get userIdToShow
  return $ case user of
    Nothing -> Nothing
    Just userProfile -> do
      Just . LayoutStub "Nutzerprofil" $
        div_
          [class_ "container p-3"]
          ( UserProfile.render
              userProfile
              (UserProfile.CanDelete userIsAdmin)
              (UserProfile.CanEdit (isOwnProfile || userIsAdmin))
          )

viewListGet ::
  ( MonadIO m,
    MonadReader env m,
    K.KatipContext m,
    App.HasDb env,
    MonadThrow m
  ) =>
  Wai.Request ->
  User.Session.Authenticated ->
  m LayoutStub
viewListGet req auth = do
  let selectionRaw = Map.findWithDefault "all" "userselect" $ parseQueryParams req

  users <- User.getAll

  selectedOption <- case User.List.makeOptionFromSelection selectionRaw of
    Left e -> throwString $ show e
    Right v -> return v

  let usersToShow = case User.List.optionValue selectedOption of
        User.List.All -> users
        User.List.Some role -> filter (elem role . User.userRoles . snd) users

  return $
    LayoutStub "Mitglieder" $
      div_ [class_ "container p-2"] $ do
        when (User.Session.isAdmin' auth) $ a_ [class_ "btn btn-sm btn-primary mb-3", href_ "/nutzer/neu"] "Neues Mitglied hinzufügen"
        h1_ [class_ "h4 mb-5"] "Mitgliederliste"
        div_ [class_ "row row-cols-1 g-2"] $
          User.List.render usersToShow selectedOption $
            filter (/= selectedOption) User.List.allOptions
