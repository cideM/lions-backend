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

module Events.EventCard (render) where

import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Events.Domain (Event (..), EventId (..), Reply (..))
import Layout (ariaLabel_)
import Locale (german)
import Lucid

render :: (EventId, Event, Maybe Reply) -> Html ()
render (EventId eventid, Event {..}, ownReply) = do
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
                name_ "numberOfGuests",
                id_ "numberOfGuests",
                value_ replyGuests',
                placeholder_ "0"
              ]
          div_ [class_ "col-sm-3 d-flex justify-content-start align-items-end"] $
            button_ [type_ "submit", class_ "btn btn-primary btn-sm"] "Speichern"
    div_ [class_ "row g-0 border-top"] $ do
      replyThing "Zusagen" (Text.pack $ show coming)
      replyThing "Absagen" (Text.pack $ show notComing)
      replyThing "Teilnehmer" (Text.pack $ show (guests + coming))
  where
    replyGuests' = case replyComing' of
      Just True -> maybe mempty (Text.pack . show . replyGuests) ownReply
      _ -> mempty
    replyComing' = fmap replyComing ownReply
    replyThing :: Text -> Text -> Html ()
    replyThing label num = do
      div_ [class_ "col-4 border-end d-flex flex-column justify-content-center align-items-center p-2"] $ do
        p_ [class_ "mb-0 text-muted"] $ toHtml label
        p_ [class_ "fw-bolder mb-0"] $ toHtml num
