module Userlist (get) where

import Control.Exception.Safe
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import Layout (ActiveNavLink (..), LayoutStub (..), ariaLabel_)
import Lucid
import qualified Network.Wai as Wai
import qualified Session.Auth as Session
import qualified Session.Types as Session
import User.DB (getUsers)
import User.Types
import Wai (parseQueryParams)

data Option a = Option {optionLabel :: Text, optionValue :: a} deriving (Show, Eq)

data Dropdown a = Dropdown {dropdownSelected :: Option a, dropdownOthers :: [Option a]} deriving (Show, Eq)

data MemberlistProfile = MemberlistProfile
  { memberlistProfileName :: Text,
    memberlistProfileProfileLink :: Text,
    memberlistProfileEmailLink :: Text,
    memberlistProfileEmail :: Text,
    memberlistProfileRoles :: [Text]
  }

render :: Show a => [MemberlistProfile] -> Dropdown a -> Html ()
render users Dropdown {..} = do
  div_ [class_ "d-flex justify-content-between align-items-center mb-3 flex-wrap"] $ do
    form_ [method_ "get", action_ "/nutzer", class_ "d-flex mb-2"] $ do
      select_ [name_ "userselect", class_ "form-select form-select-sm me-1", ariaLabel_ "Nutzergruppe auswählen"] $ do
        option_ [value_ (Text.pack . show $ optionValue dropdownSelected)] . toHtml $ optionLabel dropdownSelected
        mapM_ (\Option {..} -> option_ [value_ . Text.pack $ show optionValue] $ toHtml optionLabel) dropdownOthers
      button_ [class_ "btn btn-sm btn-secondary", type_ "submit"] "Ok"
    a_ [href_ "", class_ "btn btn-primary btn-sm disabled", role_ "button", id_ "email-button"] $ "Email abschicken"
  table_ [class_ "table mb-3", id_ "userslist"] $ do
    thead_ [] $
      tr_ [class_ "align-middle"] $ do
        th_ [scope_ "col", class_ "text-center"] "Profil"
        th_ [scope_ "col mw-50"] "Email"
        th_ [scope_ "col", class_ "d-none d-lg-table-cell"] "Name"
        th_ [scope_ "col", class_ "d-none d-lg-table-cell"] "Status"
        th_ [scope_ "col", class_ "text-center"] $ envelopeSvg
    tbody_ [] $
      mapM_
        ( \MemberlistProfile {..} -> do
            tr_ [] $ do
              td_ [class_ "text-center align-middle"] $
                a_ [href_ memberlistProfileProfileLink] personCircleSvg
              td_ [] $ a_ [href_ memberlistProfileEmailLink, class_ "text-break"] $ toHtml memberlistProfileEmail
              td_ [class_ "d-none d-lg-table-cell"] $
                unless (Text.null $ Text.strip memberlistProfileName) $ p_ [class_ "fw-bold m-0"] $ toHtml memberlistProfileName
              td_ [class_ "text-muted d-none d-lg-table-cell"] . toHtml $ Text.intercalate "," memberlistProfileRoles
              td_ [class_ "text-center align-middle"] $
                input_ [type_ "checkbox", value_ "", data_ "email" memberlistProfileEmail]
        )
        users
  where
    envelopeSvg =
      toHtmlRaw
        ( [i|
      <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" fill="currentColor" role="button" class="bi text-muted bi-envelope" id="toggle-email-button" viewBox="0 0 16 16">
        <path d="M.05 3.555A2 2 0 0 1 2 2h12a2 2 0 0 1 1.95 1.555L8 8.414.05 3.555zM0 4.697v7.104l5.803-3.558L0 4.697zM6.761 8.83l-6.57 4.027A2 2 0 0 0 2 14h12a2 2 0 0 0 1.808-1.144l-6.57-4.027L8 9.586l-1.239-.757zm3.436-.586L16 11.801V4.697l-5.803 3.546z"/>
      </svg>
      |] ::
            Text
        )
    personCircleSvg =
      toHtmlRaw
        ( [i|
      <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" fill="currentColor" class="text-muted bi bi-person-circle" viewBox="0 0 16 16">
        <path d="M11 6a3 3 0 1 1-6 0 3 3 0 0 1 6 0z"/>
        <path fill-rule="evenodd" d="M0 8a8 8 0 1 1 16 0A8 8 0 0 1 0 8zm8-7a7 7 0 0 0-5.468 11.37C3.242 11.226 4.805 10 8 10s4.757 1.225 5.468 2.37A7 7 0 0 0 8 1z"/>
      </svg>
      |] ::
            Text
        )

makeOptionFromSelection :: Text -> Either Text (Option RoleOption)
makeOptionFromSelection "all" = Right allOption
makeOptionFromSelection "admin" = Right adminOption
makeOptionFromSelection "board" = Right boardOption
makeOptionFromSelection "president" = Right presidentOption
makeOptionFromSelection "passive" = Right passiveOption
makeOptionFromSelection str = Left [i|no option found for #{str}|]

data RoleOption = All | Some Role deriving (Eq)

instance Show RoleOption where
  show All = "all"
  show (Some role) = show role

allOption, adminOption, boardOption, presidentOption, passiveOption :: Option RoleOption
allOption = Option {optionLabel = "Alle anzeigen", optionValue = All}
adminOption = Option {optionLabel = "Nur Administratoren", optionValue = Some Admin}
boardOption = Option {optionLabel = "Nur Vorstand", optionValue = Some Board}
presidentOption = Option {optionLabel = "Nur Präsident", optionValue = Some President}
passiveOption = Option {optionLabel = "Nur Passiv", optionValue = Some Passive}

allOptions :: [Option RoleOption]
allOptions = [allOption, adminOption, boardOption, presidentOption, passiveOption]

-- Turn the selected value from the incoming request into an Option and then
-- filter the users accordingly and return a proper Dropdown where the
-- selection and the other options are separated.
selectionLogic :: [UserProfile] -> Text -> Either Text ([UserProfile], Dropdown RoleOption)
selectionLogic allUsers selected = do
  parsed <- makeOptionFromSelection selected
  let others = filter (not . (==) parsed) allOptions
      usersToShow = case optionValue parsed of
        All -> allUsers
        Some role -> filter (elem role . userRoles) allUsers
  return $ (usersToShow, Dropdown {dropdownSelected = parsed, dropdownOthers = others})

-- Looks complicated but essentially turns Foo into "cool foo"
rolesToBadge :: NonEmpty Role -> [Text]
rolesToBadge = map showBadge . filter dropUser . toList
  where
    dropUser User = False
    dropUser _ = True
    showBadge President = "Präsident"
    showBadge Passive = "Passiv"
    showBadge Board = "Vorstand"
    showBadge Admin = "Administrator"
    showBadge User = "Nutzer"

formatDataForView :: [UserProfile] -> [MemberlistProfile]
formatDataForView users =
  map
    ( \UserProfile {..} ->
        let name = fromMaybe "" userFirstName <> " " <> fromMaybe "" userLastName
            (UserEmail email) = userEmail
            (UserId uid) = userId
         in MemberlistProfile
              { memberlistProfileName = name,
                memberlistProfileProfileLink = [i|/nutzer/#{uid}|],
                memberlistProfileEmailLink = [i|mailto:#{showEmail email}|],
                memberlistProfileEmail = showEmail email,
                memberlistProfileRoles = rolesToBadge userRoles
              }
    )
    users

get ::
  (MonadIO m, MonadThrow m) =>
  SQLite.Connection ->
  Wai.Request ->
  Session.Authenticated ->
  m LayoutStub
get conn req auth = do
  let selectionRaw = Map.findWithDefault "all" "userselect" $ parseQueryParams req
  users <- liftIO $ getUsers conn
  (usersToShow, dropdown) <- case selectionLogic users selectionRaw of
    Left e -> throwString $ show e
    Right v -> return v
  return $
    LayoutStub "Mitglieder" (Just Members) $
      div_ [class_ "container p-2"] $ do
        when (Session.isUserAdmin auth) $ a_ [class_ "btn btn-primary mb-3", href_ "/nutzer/neu"] "Neues Mitglied hinzufügen"
        h1_ [class_ "h4 mb-5"] "Mitgliederliste"
        div_ [class_ "row row-cols-1 g-2"] $
          render (formatDataForView usersToShow) dropdown
