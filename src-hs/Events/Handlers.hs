{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Events.Handlers where

import Capability.Reader (HasReader (..), ask)
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Events.DB (deleteReply, getAll, upsertReply)
import Events.Domain (Event (..), EventId (..), Reply (..))
import qualified Events.EventCard
import GHC.Exts (sortWith)
import Katip
import Layout (ActiveNavLink (..), ariaLabel_, layout)
import Locale (german)
import Lucid
import qualified Network.Wai as Wai
import qualified Routes.Data as Auth
import Text.Read (readEither)
import User.DB (getUser)
import User.Domain (Role (..), UserEmail (..), UserId (..), UserProfile (..), parseRole)
import Wai (parseParams)

sortByDateDesc :: [(EventId, Event)] -> [(EventId, Event)]
sortByDateDesc = sortWith (Down . (\(_, Event {..}) -> eventDate))

showAllEvents ::
  ( MonadIO m,
    HasReader "dbConn" SQLite.Connection m,
    KatipContext m
  ) =>
  Auth.Authenticated ->
  m (Html ())
showAllEvents auth = do
  let (userIsAdmin, Auth.UserSession {..}) = case auth of
        Auth.IsAdmin (Auth.AdminUser session) -> (True, session)
        Auth.IsUser session -> (False, session)
  conn <- ask @"dbConn"
  events <- map (addOwnReply userSessionUserId) . sortByDateDesc . Map.toList <$> liftIO (getAll conn)
  logLocM InfoS $ showLS events
  return . layout "Veranstaltungen" (Just Events) $
    div_ [class_ "container p-3"] $
      div_ [class_ "row row-cols-1 row-cols-lg-2 g-5"] $
        mapM_ (div_ [class_ "col"] . Events.EventCard.render) events
  where
    addOwnReply userid (eventid, e@Event {..}) = (eventid, e, find ((==) userid . replyUserId) eventReplies)

parseComing :: (MonadThrow m) => Text -> m (Maybe Bool)
parseComing "coming" = return $ Just True
parseComing "notcoming" = return $ Just False
parseComing "noreply" = return Nothing
parseComing s = throwString . Text.unpack $ "unknown 'coming' value: " <> s

parseNumGuests :: (MonadThrow m) => Text -> m Int
parseNumGuests s =
  case readEither (Text.unpack s) of
    Left e -> throwString . Text.unpack $ "couldn't parse '" <> s <> "' number of guests as number: " <> Text.pack (show e)
    Right i -> return i

replyToEvent ::
  ( MonadIO m,
    MonadThrow m,
    HasReader "dbConn" SQLite.Connection m,
    KatipContext m
  ) =>
  Wai.Request ->
  EventId ->
  Auth.Authenticated ->
  m (Html ())
replyToEvent req eventid auth = do
  let (_, Auth.UserSession {..}) = case auth of
        Auth.IsAdmin (Auth.AdminUser session) -> (True, session)
        Auth.IsUser session -> (False, session)
  conn <- ask @"dbConn"
  (_, (_, UserProfile {..})) <-
    getUser conn userSessionUserId >>= \case
      Nothing -> throwString "no user for userid from session"
      Just v -> return v
  params <- liftIO $ parseParams req
  coming <-
    maybe (throwString "no 'reply' param") parseComing $
      Map.lookup "reply" params
  numberOfGuests <-
    maybe (throwString "no 'numberOfGuests' param") parseNumGuests $
      Map.lookup "numberOfGuests" params
  case coming of
    Nothing -> deleteReply conn eventid userSessionUserId
    Just yesno -> upsertReply conn eventid (Reply yesno userEmail userSessionUserId numberOfGuests)
  return . layout "Veranstaltungen" (Just Events) $
    div_ [class_ "container p-3"] "hi"
