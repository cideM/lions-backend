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
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import qualified Data.Map.Strict as Map

-- import qualified Data.Time as Time

-- import qualified Network.Wai as Wai

import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Events.DB (getAll)
import Events.Domain (Event (..), EventId (..), Reply (..))
import GHC.Exts (sortWith)
import Katip
import Layout (ActiveNavLink (..), ariaLabel_, layout)
import Locale (german)
import Lucid
import qualified Routes.Data as Auth
import User.Domain (UserId (..))

sortByDateDesc :: [(EventId, Event)] -> [(EventId, Event)]
sortByDateDesc = sortWith (Down . (\(_, Event {..}) -> eventDate))

renderEvent :: (EventId, Event, Maybe Reply) -> Html ()
renderEvent (EventId eventid, Event {..}, ownReply) = do
  let formatted = Text.pack . Time.formatTime german "%A, %d. %B %Y %R %p" $ eventDate
      coming = eventReplies & filter replyComing & length
      notComing = eventReplies & filter (not . replyComing) & length
      guests = eventReplies & filter replyComing & map replyGuests & sum
  div_ [class_ "card"] $ do
    div_ [class_ "card-body"] $ do
      h1_ [class_ "card-title fs-4"] $ toHtml eventTitle
      h2_ [class_ "card-subtitle fs-5 mb-3 text-muted"] $ toHtml formatted
      p_ [class_ "card-text"] $ toHtml eventDescription
      form_
        [ class_ "row my-2 g-3",
          method_ "post",
          action_ ("/veranstaltungen/" <> Text.pack (show eventid) <> "/antwort")
        ]
        $ do
          div_ [class_ "col-sm-5"] $ do
            label_ [class_ "form-label text-muted", for_ "replySelect"] "Antwort"
            select_
              [ name_ "reply",
                id_ "replySelect",
                class_ "form-select form-select-sm me-1",
                ariaLabel_ "Veranstaltung beantworten"
              ]
              $ do
                option_ (value_ "coming" : [selected_ "selected" | replyComing' == Just True]) "Zusage"
                option_ (value_ "notcoming" : [selected_ "selected" | replyComing' == Just False]) "Absage"
                option_ (value_ "noreply" : [selected_ "selected" | isNothing replyComing']) "Keine Antwort"
          div_ [class_ "col-sm-4"] $ do
            label_ [class_ "form-label text-muted", for_ "numberOfGuests"] "Anzahl Gäste"
            input_
              [ class_ "form-control form-control-sm",
                type_ "number",
                id_ "numberOfGuests",
                value_ replyGuests',
                placeholder_ "0"
              ]
          div_ [class_ "col-sm-3 d-flex justify-content-start align-items-end"] $
            button_ [type_ "submit", class_ "btn btn-primary btn-sm"] "Speichern"
    div_ [class_ "row g-0 border-top"] $ do
      replyThing "Zusagen" (Text.pack $ show coming)
      replyThing "Absagen" (Text.pack $ show notComing)
      replyThing "Gäste" (Text.pack $ show guests)
  where
    replyGuests' = maybe mempty (Text.pack . show . replyGuests) ownReply
    replyComing' = fmap replyComing ownReply
    replyThing :: Text -> Text -> Html ()
    replyThing label num = do
      div_ [class_ "col-4 border-end d-flex flex-column justify-content-center align-items-center p-2"] $ do
        p_ [class_ "mb-0 text-muted"] $ toHtml label
        p_ [class_ "fw-bolder mb-0"] $ toHtml num

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
        mapM_ (div_ [class_ "col"] . renderEvent) events
  where
    addOwnReply userid (eventid, e@Event {..}) = (eventid, e, find ((==) userid . replyUserId) eventReplies)
