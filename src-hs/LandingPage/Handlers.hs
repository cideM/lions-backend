{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LandingPage.Handlers (showLandingPage) where

import Control.Exception.Safe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import LandingPage.UsersList (UserGroupToShow (..))
import qualified LandingPage.UsersList
import Layout (ActiveNavLink (..), layout)
import Lucid
import qualified System.Log.FastLogger as Log
import User.DB (getUsers)
import User.Domain (Role (..), isAdmin)
import qualified Web.Scotty as S
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
  S.ActionM ()
showLandingPage _ conn roles = do
  selectionRaw <- S.param "userselect" `S.rescue` const (return "all")
  selectionParsed <- case parseSelection selectionRaw of
    Left e -> throwString . Text.unpack $ "invalid group selection: " <> selectionRaw <> " " <> e
    Right (v :: UserGroupToShow) -> pure v
  msg <- S.liftAndCatchIO $ getWelcomeMsgFromDb conn
  zone <- S.liftAndCatchIO Time.getCurrentTimeZone
  users <- S.liftAndCatchIO $ getUsers conn
  let msg' = (\(WelcomeMsg content datetime) -> (content, Time.utcToZonedTime zone datetime)) <$> msg
      userIsAdmin = any isAdmin roles
      usersToShow = case selectionParsed of
        All -> users
        Some role -> filterUsers role users
  S.html . renderText $ do
    layout "Willkommen" (Just Welcome) $
      div_ [class_ "container"] $
        div_ [class_ "row g-5"] $ do
          section_ [class_ "justify-content-center col-md-6"] (WelcomeMessage.Card.render msg' userIsAdmin)
          section_ [class_ "justify-content-center col-md-6"] (LandingPage.UsersList.render usersToShow userIsAdmin selectionParsed)
  where
    filterUsers keep = filter (elem keep . fst)
