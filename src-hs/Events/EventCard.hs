module Events.EventCard (render) where

import Control.Monad (when)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Events.Domain (Event (..), EventId (..), Reply (..))
import Locale (german)
import Lucid

-- TODO: Why the tuples?
render :: (EventId, Event, Maybe Reply) -> Html ()
render (EventId eventid, Event {..}, ownReply) = do
  let formatted = Text.pack . Time.formatTime german "%A, %d. %B %Y %R %p" $ eventDate
      coming = eventReplies & filter replyComing & length
      notComing = eventReplies & filter (not . replyComing) & length
      guests = eventReplies & filter replyComing & map replyGuests & sum
  div_ [class_ "card"] $ do
    div_ [class_ "card-header"] $ do
      span_ [] $ toHtml formatted
      when eventFamilyAllowed $ do span_ [class_ "ms-2 badge bg-primary"] "Mit Familie"
      when ((replyComing <$> ownReply) == Just True) $ do span_ [class_ "ms-2 badge bg-success text-white"] "Zugesagt"
      when ((replyComing <$> ownReply) == Just False) $ do span_ [class_ "ms-2 badge bg-danger text-white"] "Abgesagt"
    div_ [class_ "card-body"] $ do
      a_ [href_ $ "/veranstaltungen/" <> (Text.pack $ show eventid)] $ h1_ [class_ "card-title fs-4"] $ toHtml eventTitle
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
