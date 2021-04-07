{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LandingPage.Handlers (showLandingPage) where

import Control.Exception.Safe
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import LandingPage.UsersList (UserGroupToShow (..))
import qualified LandingPage.UsersList
import Layout (ActiveNavLink (..), layout)
import Lucid
import qualified Network.Wai as Wai
import qualified System.Log.FastLogger as Log
import User.DB (getUsers)
import User.Domain (Role (..), isAdmin)
import Wai (parseParams)
import qualified WelcomeMessage.Card
import WelcomeMessage.DB (getWelcomeMsgFromDb)
import WelcomeMessage.Domain (WelcomeMsg (..))

parseSelection :: Text -> Either Text UserGroupToShow
parseSelection "all" = Right All
parseSelection "admin" = Right $ Some Admin
parseSelection "board" = Right $ Some Board
parseSelection "user" = Right $ Some User
parseSelection "president" = Right $ Some President
parseSelection v = Left $ "unknown user group: " <> v

showLandingPage ::
  Log.FastLogger ->
  SQLite.Connection ->
  [Role] ->
  Wai.Request ->
  IO (Html ())
showLandingPage _ conn roles req = do
  params <- parseParams req
  let selectionRaw = Map.findWithDefault "all" "userselect" params
  selectionParsed <- case parseSelection selectionRaw of
    Left e -> throwString . Text.unpack $ "invalid group selection: " <> selectionRaw <> " " <> e
    Right (v :: UserGroupToShow) -> pure v
  msg <- getWelcomeMsgFromDb conn
  zone <- Time.getCurrentTimeZone
  users <- getUsers conn
  let msg' = (\(WelcomeMsg content datetime) -> (content, Time.utcToZonedTime zone datetime)) <$> msg
      userIsAdmin = any isAdmin roles
      usersToShow = case selectionParsed of
        All -> users
        Some role -> filterUsers role users
  return $
    layout "Willkommen" (Just Welcome) $
      div_ [class_ "container"] $
        div_ [class_ "row g-5"] $ do
          section_ [class_ "justify-content-center col-md-6"] (WelcomeMessage.Card.render msg' userIsAdmin)
          section_ [class_ "justify-content-center col-md-6"] (LandingPage.UsersList.render usersToShow userIsAdmin selectionParsed)
  where
    filterUsers keep = filter (elem keep . fst)
