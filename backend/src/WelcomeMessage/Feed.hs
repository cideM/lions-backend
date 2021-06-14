module WelcomeMessage.Feed (renderFeed) where

import Control.Monad (when)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Layout (ActiveNavLink (..), LayoutStub(..))
import Locale (german)
import Lucid
import WelcomeMessage.Types
import Prelude hiding (id)

type ShowEditBtn = Bool

newtype EditHref = EditHref Text

newtype DeleteHref = DeleteHref Text

renderSingleMessage :: EditHref -> DeleteHref -> (Text, Time.ZonedTime) -> ShowEditBtn -> Html ()
renderSingleMessage (EditHref editHref) (DeleteHref deleteHref) (msg, date) canEdit =
  article_ [class_ ""] $ do
    let formatted = Time.formatTime german "%A, %d. %B %Y" date
     in do
          h2_ [class_ "h4"] $ toHtml formatted
          p_ [class_ "", style_ "white-space: pre-wrap"] $ toHtml msg
          when canEdit $
            div_ [class_ "d-flex"] $ do
              a_ [class_ "link-primary me-3", href_ editHref] "Ändern"
              a_ [class_ "link-danger me-3", href_ deleteHref] "Löschen"

renderFeed :: Time.TimeZone -> Bool -> [WelcomeMsg] -> LayoutStub
renderFeed zone userIsAdmin msgs =
  LayoutStub "Willkommen" (Just Welcome) $
    div_ [class_ "container"] $ do
      div_ [class_ "row row-cols-1 g-4"] $ do
        div_ [class_ "col"] $ do
          p_ [class_ "m-0 alert alert-secondary"] $ do
            "Alle Dateien (inklusive Bilderarchiv) des Lions Club Achern befinden sich auf "
            a_ [href_ "https://1drv.ms/f/s!As3H-io1fRdFcZnEJ0BXdpeV9Lw"] "Microsoft OneDrive"
          when userIsAdmin $
            p_ [class_ "mt-3 alert alert-secondary"] $ do
              "Mit diesem Link "
              a_ [href_ "https://1drv.ms/f/s!As3H-io1fRdFcUPc-Dz3SC08Wno"] "(Microsoft OneDrive)"
              [i|
              können die Dateien im geteilten Ordner "Lions Dateien" bearbeitet
              werden. Dieser Link ist nur für Administratoren gedacht und wird
              auch nur Administratoren angezeigt. Zum Bearbeiten ist jedoch ein
              Microsoft Account notwendig!
              |]
        div_ [class_ "col d-flex flex-wrap-reverse align-items-center"] $ do
          h1_ [class_ "h3 m-0 me-2 mb-1"] "Interne Neuigkeiten"
          when userIsAdmin $
            a_ [class_ "btn btn-primary mb-1", href_ "/neu", role_ "button"] "Neue Nachricht"
        div_ [class_ "col"] $ do
          div_ [class_ "row row-cols-1 g-5"] $ do
            mapM_
              ( \(WelcomeMsg (WelcomeMsgId id) content datetime) ->
                  let editHref = EditHref $ Text.pack $ "/editieren/" <> show id
                      deleteHref = DeleteHref $ Text.pack $ "/loeschen/" <> show id
                      zoned = Time.utcToZonedTime zone datetime
                   in (renderSingleMessage editHref deleteHref (content, zoned) userIsAdmin)
              )
              msgs
