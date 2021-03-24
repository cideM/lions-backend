{-# LANGUAGE OverloadedStrings #-}

module WelcomeMessage.Card (render) where

import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Time as Time
import Locale (german)
import Lucid

type ShowEditBtn = Bool

render :: Maybe (Text, Time.ZonedTime) -> ShowEditBtn -> Html ()
render welcome canEdit =
  div_ [class_ "card text-dark bg-light"] $ do
    div_ [class_ $ "card-header" <> (if isJust welcome then "" else " mb-0")] "Interne Neuigkeiten"
    case welcome of
      Nothing -> mempty
      Just (msg, date) -> do
        let formatted = Time.formatTime german "%A, %d. %B %Y" date
         in div_ [class_ "card-body"] $ do
              h1_ [class_ "card-title fs-4"] $ toHtml formatted
              p_ [class_ "card-text", style_ "white-space: pre"] $ toHtml msg
              when canEdit $ a_ [class_ "btn btn-primary", href_ "/edit"] "Nachricht ändern"
