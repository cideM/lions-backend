module Events.SingleEvent (render, ShowAdminTools (..)) where

import Control.Monad (forM_, when)
import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.String.Interpolate (i)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Events.Domain (Event (..), EventAttachment (..), EventId (..), Reply (..))
import Layout (ariaLabel_)
import Locale (german)
import Lucid
import User.Types (UserEmail (..), UserId (..), showEmail)

newtype ShowAdminTools = ShowAdminTools Bool deriving (Show)

render :: ShowAdminTools -> Maybe Reply -> EventId -> Event -> Html ()
render (ShowAdminTools showAdminTools) ownReply (EventId eventId) Event {..} =
  let date = Text.pack . Time.formatTime german "%A, %d. %B %Y %R %p" $ eventDate
      coming = eventReplies & filter replyComing & length
      notComing = eventReplies & filter (not . replyComing) & length
      guests = eventReplies & filter replyComing & map replyGuests & sum
   in do
        div_ [class_ "container"] $ do
          div_ [class_ "row gy-3 gx-lg-4 mb-5"] $ do
            when showAdminTools $ do
              div_ [class_ "col-12 d-flex flex-wrap"] $ do
                a_
                  [ class_ "btn btn-sm btn-danger me-4",
                    role_ "button",
                    href_ . Text.pack $ "/veranstaltungen/" <> show eventId <> "/loeschen"
                  ]
                  "Löschen"
                a_
                  [ class_ "btn btn-sm btn-secondary",
                    role_ "button",
                    href_ . Text.pack $ "/veranstaltungen/" <> show eventId <> "/editieren"
                  ]
                  "Editieren"
            section_ [class_ "justify-content-center col-lg-8"] $ do
              div_ [class_ "mb-1 text-muted"] $ do
                span_ [class_ "me-2"] $ toHtml date
                when eventFamilyAllowed $ span_ [class_ "badge bg-success"] "Mit Familie"
              p_ [class_ "mb-1 text-muted"] $ toHtml $ "Ort: " <> eventLocation
              h1_ [class_ "mb-2 h3"] $ toHtml eventTitle
              p_ [class_ "my-3"] $ toHtml eventDescription
              when (length eventAttachments > 0) $ do
                p_ [class_ "m-0"] "Angehängte Dateien: "
                ul_ [] $ do
                  forM_
                    eventAttachments
                    ( \EventAttachment {..} ->
                        li_ [] $
                          a_ [href_ [i|/#{eventId}/#{eventAttachmentFileName}|]] $ toHtml eventAttachmentFileName
                    )
            section_ [class_ "justify-content-center col-lg-4"] $ do
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
                        label_ [class_ "form-label", for_ "numberOfGuests"] "Anzahl Gäste die du mitbringst"
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
          div_ [class_ "row row-cols-1 row-cols-lg-2 mt-4 gy-4 gx-lg-4"] $ do
            div_ [class_ "col"] $ do
              when (coming > 0) $ do
                h2_ [class_ "h4"] "Zusagen"
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
              when (notComing > 0) $ do
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
