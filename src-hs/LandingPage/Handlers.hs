{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LandingPage.Handlers (showLandingPage) where

import Capability.Reader (HasReader (..), ask)
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Layout (ActiveNavLink (..), layout)
import Lucid
import qualified Routes.Data as Auth
import Prelude hiding (id)
import qualified WelcomeMessage.Card
import WelcomeMessage.DB (getAllWelcomeMsgsFromDb)
import WelcomeMessage.Domain (WelcomeMsg (..), WelcomeMsgId (..))

showLandingPage ::
  ( MonadIO m,
    MonadCatch m,
    HasReader "dbConn" SQLite.Connection m
  ) =>
  Auth.Authenticated ->
  m (Html ())
showLandingPage auth = do
  let userIsAdmin = case auth of
        Auth.IsAdmin _ -> True
        _ -> False
  conn <- ask @"dbConn"
  msgs <-
    handleAny (\e -> throwString $ "error getting welcome messages: " <> show e) $
      getAllWelcomeMsgsFromDb conn
  zone <- liftIO Time.getCurrentTimeZone
  return $
    layout "Willkommen" (Just Welcome) $
      div_ [class_ "container"] $ do
        p_ [class_ "alert alert-info"] $ do
          "Alle Dateien (inklusive Bilderarchiv) des Lions Club Achern befinden sich auf "
          a_ [href_ "https://1drv.ms/f/s!As3H-io1fRdFcZnEJ0BXdpeV9Lw"] "Microsoft OneDrive"
        when userIsAdmin $
          a_ [class_ "mb-3 btn btn-primary", href_ "/neu", role_ "button"] "Neue Nachricht"
        h1_ [class_ "display-6 mb-3"] "Interne Neuigkeiten"
        div_ [class_ "row row-cols-1 g-5"] $ do
          mapM_
            ( \(WelcomeMsg (WelcomeMsgId id) content datetime) ->
                let editHref = WelcomeMessage.Card.EditHref $ Text.pack $ "/editieren/" <> show id
                    deleteHref = WelcomeMessage.Card.DeleteHref $ Text.pack $ "/loeschen/" <> show id
                    zoned = Time.utcToZonedTime zone datetime
                 in section_
                      [class_ "justify-content-center col"]
                      (WelcomeMessage.Card.render editHref deleteHref (content, zoned) userIsAdmin)
            )
            msgs
