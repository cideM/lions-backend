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

module Events.SingleEvent (render, ShowAdminTools (..)) where

import Control.Monad (when)
import Data.Function ((&))
import Data.Maybe (isNothing)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Events.Domain (Event (..), EventId (..), Reply (..))
import Layout (ariaLabel_)
import Locale (german)
import Lucid
import User.Domain (UserEmail (..), UserId (..), showEmail)

newtype ShowAdminTools = ShowAdminTools Bool deriving (Show)

render :: ShowAdminTools -> Maybe Reply -> EventId -> Event -> Html ()
render (ShowAdminTools showAdminTools) ownReply (EventId eventId) Event {..} =
  let formatted = Text.pack . Time.formatTime german "%A, %d. %B %Y %R %p" $ eventDate
      coming = eventReplies & filter replyComing & length
      guests = eventReplies & filter replyComing & map replyGuests & sum
   in do
        div_ [class_ "container"] $ do
          div_ [class_ "row row-cols-1 row-cols-lg-2 g-5 mb-5"] $ do
            section_ [class_ "justify-content-center col"] $ do
              div_ [class_ "card"] $ do
                div_ [class_ "card-header d-flex align-items-center"] $ do
                  p_ [class_ "m-0"] $ toHtml formatted
                  when eventFamilyAllowed $ do span_ [class_ "ms-2 badge bg-primary"] "Mit Familie"
                div_ [class_ "card-body"] $ do
                  h1_ [class_ "card-title h2"] $ toHtml eventTitle
                  h2_ [class_ "card-subtitle h5 mb-3 text-muted"] $ toHtml $ "Ort: " <> eventLocation
                  p_ [class_ "card-text"] $ toHtml eventDescription
              when showAdminTools $ do
                div_ [class_ "px-1 mt-2 d-flex flex-wrap"] $ do
                  a_
                    [ class_ "link-danger me-4",
                      href_ . Text.pack $ "/Veranstaltung/" <> show eventId <> "/löschen"
                    ]
                    "Veranstaltung löschen"
                  a_
                    [ class_ "link-secondary",
                      href_ . Text.pack $ "/Veranstaltung/" <> show eventId <> "/editieren"
                    ]
                    "Veranstaltung editieren"
            section_ [class_ "justify-content-center col"] $ do
              div_ [class_ "card"] $ do
                div_ [class_ "card-header"] $ do
                  span_ "Deine Antwort"
                div_ [class_ "card-body"] $ do
                  form_
                    [ class_ "my-2 g-3",
                      method_ "post",
                      action_ ("/veranstaltungen/" <> Text.pack (show eventId) <> "/antwort")
                    ]
                    $ do
                      div_ [class_ "mb-3"] $ do
                        label_ [class_ "form-label", for_ "replySelect"] "Antwort"
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
                      div_ [class_ "mb-3"] $ do
                        label_ [class_ "form-label", for_ "numberOfGuests"] "Anzahl Gäste"
                        input_
                          [ class_ "form-control form-control-sm",
                            type_ "number",
                            name_ "numberOfGuests",
                            id_ "numberOfGuests",
                            value_ replyGuests',
                            placeholder_ "0"
                          ]
                      div_ [class_ "d-flex justify-content-start align-items-end"] $
                        button_ [type_ "submit", class_ "btn btn-primary btn-sm"] "Speichern"
          div_ [class_ "row row-cols-1 row-cols-lg-2 g-5"] $ do
            div_ [class_ "col"] $ do
              h2_ [class_ "h4"] "Zusagen"
              when (coming > 0) $ do
                div_ [class_ "alert alert-secondary my-3", role_ "alert"] $ do
                  div_ [class_ "d-flex justify-content-between"] $ do
                    span_ "Lions Mitglieder: "
                    span_ $ toHtml (show coming)
                  div_ [class_ "d-flex justify-content-between"] $ do
                    span_ "Gäste: "
                    span_ $ toHtml (show guests)
                  div_ [class_ "d-flex justify-content-between"] $ do
                    span_ "Insgesamt: "
                    span_ $ toHtml (show (coming + guests))
              table_ [class_ "table"] $ do
                thead_ $ do
                  tr_ $ do
                    th_ [scope_ "col"] "Email"
                    th_ [scope_ "col"] "Gäste"
                    th_ [scope_ "col"] ""
                tbody_ $ do
                  mapM_
                    ( \Reply {replyEmail = UserEmail email, replyUserId = UserId userid, ..} -> do
                        tr_ $ do
                          td_ [] $ toHtml $ showEmail email
                          td_ [] $ toHtml $ show replyGuests
                          td_ [class_ "d-flex justify-content-end"] $
                            a_ [href_ . Text.pack $ "/nutzer/" <> show userid] "Zum Profil"
                    )
                    (eventReplies & filter replyComing)
            div_ [class_ "col"] $ do
              h2_ [class_ "h4"] "Absagen"
              table_ [class_ "table"] $ do
                thead_ $ do
                  tr_ $ do
                    th_ [scope_ "col"] "Email"
                    th_ [scope_ "col"] ""
                tbody_ $ do
                  mapM_
                    ( \Reply {replyEmail = UserEmail email, replyUserId = UserId userid} -> do
                        tr_ $ do
                          td_ [] $ toHtml $ showEmail email
                          td_ [class_ "d-flex justify-content-end"] $
                            a_ [href_ . Text.pack $ "/nutzer/" <> show userid] "Zum Profil"
                    )
                    (eventReplies & filter (not . replyComing))
  where
    replyGuests' = case replyComing' of
      Just True -> maybe mempty (Text.pack . show . replyGuests) ownReply
      _ -> mempty
    replyComing' = fmap replyComing ownReply
