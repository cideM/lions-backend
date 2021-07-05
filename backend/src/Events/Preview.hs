module Events.Preview (render) where

import Control.Monad (when)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Data.String.Interpolate (i)
import qualified Events.Types as Events
import Locale (german)
import Lucid

render :: (Events.Id, Events.Event, Maybe Events.Reply) -> Html ()
render (Events.Id eventid, Events.Event {..}, ownReply) = do
  let formatted = Text.pack . Time.formatTime german "%A, %d. %B %Y %R %p" $ eventDate
      coming = eventReplies & filter Events.replyComing & length
      notComing = eventReplies & filter (not . Events.replyComing) & length
      guests = eventReplies & filter Events.replyComing & map Events.replyGuests & sum
  div_ [class_ "card"] $ do
    div_ [class_ "card-header"] $ do
      span_ [class_ "me-2"] $ toHtml formatted
      when eventFamilyAllowed $ span_ [class_ "badge bg-success"] "Mit Familie"
      when ((Events.replyComing <$> ownReply) == Just True) $ do span_ [class_ "ms-2 badge bg-success text-white"] "Zugesagt"
      when ((Events.replyComing <$> ownReply) == Just False) $ do span_ [class_ "ms-2 badge bg-danger text-white"] "Abgesagt"
    div_ [class_ "card-body"] $ do
      a_ [href_ [i|/veranstaltungen/#{eventid}|]] $ h1_ [class_ "card-title fs-4 mb-3"] $ toHtml eventTitle
      h2_ [class_ "card-subtitle fs-6 mb-3 text-muted"] $ toHtml $ "Ort: " <> eventLocation
      p_ [class_ "card-text"] $ toHtml eventDescription
    div_ [class_ "row g-0 border-top"] $ do
      replyThing "Zusagen" (Text.pack $ show coming)
      replyThing "Absagen" (Text.pack $ show notComing)
      replyThing "Teilnehmer" (Text.pack $ show (guests + coming))
  where
    replyThing :: Text -> Text -> Html ()
    replyThing label num = do
      div_ [class_ "col-4 border-end d-flex flex-column justify-content-center align-items-center p-2"] $ do
        p_ [class_ "mb-0 text-muted"] $ toHtml label
        p_ [class_ "fw-bolder mb-0"] $ toHtml num
