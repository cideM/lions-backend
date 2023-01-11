module Events.Event.Preview (render, IsExpired (..)) where

import Control.Monad (when)
import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Events.Event.Event as Events
import qualified Events.Event.Id as Event
import qualified Events.Reply.Reply as Event
import Locale (german)
import Lucid

newtype IsExpired = IsExpired Bool deriving (Show)

render :: (Event.Id, Events.Event, Maybe Event.Reply, IsExpired) -> Html ()
render (Event.Id eventid, Events.Event {..}, ownReply, IsExpired isExpired) = do
  let dateFormatted = Text.pack . Time.formatTime german "%A, %d. %B %Y %R %p" $ eventDate
      coming = eventReplies & filter Event.replyComing & length
      notComing = eventReplies & filter (not . Event.replyComing) & length
      guests = eventReplies & filter Event.replyComing & map Event.replyGuests & sum
      youreComing = (Event.replyComing <$> ownReply) == Just True
      youreNotComing = (Event.replyComing <$> ownReply) == Just False
      cardClass = case Event.replyComing <$> ownReply of
        Just True -> "card border-success"
        Just False -> "card border-danger"
        _ -> "card"

  div_ [class_ cardClass] $ do
    div_ [class_ "card-header"] $ do
      span_ [class_ "me-2"] $ toHtml dateFormatted
      when isExpired $ span_ [class_ "badge bg-warning text-dark me-2"] "Bereits stattgefunden"
      when eventFamilyAllowed $ span_ [class_ "badge bg-secondary me-2"] "Mit Familie"
      when youreComing $ do span_ [class_ "badge bg-success text-white me-2"] "Zugesagt"
      when youreNotComing $ do span_ [class_ "badge bg-danger text-white"] "Abgesagt"
    div_ [class_ "card-body"] $ do
      h1_ [class_ "card-title h5"] $
        a_ [href_ [i|/veranstaltungen/#{eventid}|]] $
          toHtml eventTitle
      h2_ [class_ "card-subtitle text-muted h6"] $ toHtml $ "Ort: " <> eventLocation
      p_ [class_ "card-text"] ""
      when (coming > 0 || notComing > 0) $
        div_ [class_ "d-flex"] $ do
          when (coming > 0) $
            span_ [class_ "badge rounded-pill bg-success me-2"] $
              toHtml $
                ([i|Teilnehmer: #{coming + guests}|] :: Text)
          when (notComing > 0) $
            span_ [class_ "badge rounded-pill bg-danger"] $
              toHtml $
                ([i|Absagen: #{notComing}|] :: Text)
