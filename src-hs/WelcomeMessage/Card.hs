{-# LANGUAGE OverloadedStrings #-}

module WelcomeMessage.Card (render, DeleteHref (..), EditHref (..)) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Time as Time
import Locale (german)
import Lucid

type ShowEditBtn = Bool

newtype EditHref = EditHref Text

newtype DeleteHref = DeleteHref Text

render :: EditHref -> DeleteHref -> (Text, Time.ZonedTime) -> ShowEditBtn -> Html ()
render (EditHref editHref) (DeleteHref deleteHref) (msg, date) canEdit =
  div_ [class_ "card"] $ do
    let formatted = Time.formatTime german "%A, %d. %B %Y" date
     in do
          div_ [class_ "card-header"] $ toHtml formatted
          div_ [class_ "card-body"] $ do
            p_ [class_ "card-text", style_ "white-space: pre"] $ toHtml msg
          when canEdit $
            div_ [class_ "card-footer"] $ do
              a_ [class_ "link-primary me-3", href_ editHref] "Ändern"
              a_ [class_ "link-danger me-3", href_ deleteHref] "Löschen"
