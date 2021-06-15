module User.List.Handler where

import Control.Exception.Safe
import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Database.SQLite.Simple as SQLite
import Layout (ActiveNavLink (..), LayoutStub (..))
import Lucid
import qualified Network.Wai as Wai
import qualified Routes.Data as Auth
import User.DB (getUsers)
import User.List.View
import User.Types
import Wai (parseQueryParams)

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

get :: SQLite.Connection -> Wai.Request -> Auth.Authenticated -> IO LayoutStub
get conn req auth = do
  let selectionRaw = Map.findWithDefault "all" "userselect" $ parseQueryParams req
  users <- getUsers conn
  (usersToShow, dropdown) <- case selectionLogic users selectionRaw of
    Left e -> throwString $ show e
    Right v -> return v
  return $
    LayoutStub "Mitglieder" (Just Members) $
      div_ [class_ "container p-2"] $ do
        when (userIsAdmin auth) $ a_ [class_ "btn btn-primary mb-3", href_ "/nutzer/neu"] "Neues Mitglied hinzufügen"
        h1_ [class_ "h4 mb-5"] "Mitgliederliste"
        div_ [class_ "row row-cols-1 g-2"] $
          User.List.View.render (formatDataForView usersToShow) dropdown
  where
    userIsAdmin (Auth.IsAdmin _) = True
    userIsAdmin _ = False
