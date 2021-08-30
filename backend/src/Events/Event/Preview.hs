module Events.Event.Preview (render, IsExpired (..)) where

import Control.Monad (when)
import Data.String.Interpolate (i)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Events.Attachments.Saved as Saved
import qualified Events.Event.Event as Events
import qualified Events.Event.Id as Event
import qualified Events.Reply.Reply as Event
import Locale (german)
import Lucid

newtype IsExpired = IsExpired Bool deriving (Show)

render :: (Event.Id, Events.Event Saved.FileName, Maybe Event.Reply, IsExpired) -> Html ()
render (Event.Id eventid, Events.Event {..}, ownReply, IsExpired isExpired) = do
  let formatted = Text.pack . Time.formatTime german "%A, %d. %B %Y %R %p" $ eventDate

  div_ [class_ "mb-2"] $ do
    span_ [class_ "me-2"] $ toHtml formatted
    when isExpired $ span_ [class_ "badge bg-warning text-dark me-2"] "Bereits stattgefunden"
    when eventFamilyAllowed $ span_ [class_ "badge bg-secondary me-2"] "Mit Familie"
    when ((Event.replyComing <$> ownReply) == Just True) $ do span_ [class_ "badge bg-success text-white me-2"] "Zugesagt"
    when ((Event.replyComing <$> ownReply) == Just False) $ do span_ [class_ "badge bg-danger text-white"] "Abgesagt"
  p_ [class_ "mb-3 fs-4", style_ "max-width: 48rem"] $
    a_ [href_ [i|/veranstaltungen/#{eventid}|]] $ toHtml eventTitle
  h2_ [class_ "card-subtitle fs-6 mb-3 text-muted"] $ toHtml $ "Ort: " <> eventLocation
