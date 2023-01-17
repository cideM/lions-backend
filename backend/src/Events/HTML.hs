module Events.HTML where

import qualified CMarkGFM
import Control.Monad (forM_, when)
import Data.List (partition)
import Data.Maybe (isNothing)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import Events.Event
import Events.Reply
import Form (FormFieldState (..))
import Layout (ariaCurrent_, ariaLabel_, describedBy_, infoBox)
import Locale (german)
import Lucid
import qualified Network.HTTP.Types.URI as URI
import qualified Text.HTML.SanitizeXSS as SanitizeXSS
import Prelude hiding (id)

-- eventForm renders the form for creating and updating events. The "checkboxes"
-- parameter is a list of key/value pairs, where the key should be the
-- filename.
eventForm :: Text -> Text -> [(Text, Bool)] -> FormInput -> FormState -> Html ()
eventForm btnLabel action checkboxes FormInput {..} FormState {..} = do
  form_ [class_ "row g-4", action_ action, method_ "post", enctype_ "multipart/form-data"] $ do
    div_ [class_ "col-md-6"] $ do
      let (className, errMsg) = case createEventStateTitle of
            NotValidated -> ("", Nothing)
            Valid _ -> ("is-valid", Nothing)
            Invalid msg -> ("is-invalid", Just msg)
      label_ [for_ "eventTitleInput", class_ "form-label"] "Name"
      input_
        [ type_ "text",
          id_ "eventTitleInput",
          required_ "required",
          name_ "eventTitleInput",
          class_ $ "form-control " <> className,
          describedBy_ "eventTitleFeedback",
          value_ createEventInputTitle
        ]
      maybe mempty (div_ [id_ "eventTitleFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-6"] $ do
      let (className, errMsg) = case createEventStateLocation of
            NotValidated -> ("", Nothing)
            Valid _ -> ("is-valid", Nothing)
            Invalid msg -> ("is-invalid", Just msg)
      label_ [for_ "eventLocationInput", class_ "form-label"] "Ort"
      input_
        [ type_ "text",
          class_ $ "form-control " <> className,
          id_ "eventLocationInput",
          value_ createEventInputLocation,
          required_ "required",
          describedBy_ "eventLocationFeedback",
          name_ "eventLocationInput"
        ]
      maybe mempty (div_ [id_ "eventLocationFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-6 d-flex flex-column"] $ do
      let (className, errMsg) = case createEventStateDate of
            NotValidated -> ("", Nothing)
            Valid _ -> ("is-valid", Nothing)
            Invalid msg -> ("is-invalid", Just msg)
      label_ [for_ "eventDateInput", class_ "form-label"] "Datum"
      input_
        [ type_ "text",
          id_ "eventDateInput",
          value_ createEventInputDate,
          class_ $ "form-control " <> className,
          required_ "required",
          name_ "eventDateInput",
          pattern_ "\\d{2}.\\d{2}.\\d{4} \\d{2}:\\d{2}",
          describedBy_ "eventDateHelp eventDateFeedback"
        ]
      div_ [id_ "eventDateHelp", class_ "form-text"] "Bitte als Format '12.01.2022 15:00' verwenden."
      maybe mempty (div_ [id_ "eventDateFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-12 d-flex flex-column"] $ do
      let (className, errMsg) = case createEventStateDescription of
            NotValidated -> ("", Nothing)
            Valid _ -> ("is-valid", Nothing)
            Invalid msg -> ("is-invalid", Just msg)
      label_ [for_ "eventDescriptionInput", class_ "form-label"] "Beschreibung"
      textarea_
        [ type_ "textfield",
          name_ "eventDescriptionInput",
          class_ $ "form-control " <> className,
          id_ "eventDescriptionInput",
          autofocus_,
          rows_ "10",
          describedBy_ "eventDescriptionFeedback",
          cols_ "10"
        ]
        $ toHtml createEventInputDescription
      maybe mempty (div_ [id_ "eventDescriptionFeedback", class_ "invalid-feedback"] . toHtml) errMsg
    div_ [class_ "col-md-6"] $ do
      label_ [for_ "eventAttachmentsInput", class_ "form-label"] "Dateien"
      input_
        [ type_ "file",
          multiple_ "multiple",
          class_ "form-control",
          id_ "eventAttachmentsInput",
          name_ "eventAttachmentsInput"
        ]
    div_ [class_ "col-md-6"] $
      div_ [class_ "form-check"] $ do
        input_
          ( [ class_ "form-check-input",
              type_ "checkbox",
              id_ "eventFamAllowedInput",
              name_ "eventFamAllowedInput"
            ]
              ++ [checked_ | createEventInputFamilyAllowed]
          )
        label_ [class_ "form-check-label", for_ "eventFamAllowedInput"] "Mit Familie?"
    div_ [class_ "col-md-12"] $
      when (length checkboxes > 0) $ do
        mapM_
          ( \(name, checked) ->
              div_ [class_ "form-check"] $ do
                input_ $
                  [ type_ "checkbox",
                    class_ "form-check-input",
                    id_ name,
                    name_ "newFileCheckbox",
                    value_ name
                  ]
                    ++ [checked_ | checked]
                label_ [for_ name, class_ "form-check-label"] $ toHtml name
          )
          checkboxes
    div_ [class_ "col-md-4"] $
      button_ [class_ "btn btn-primary", type_ "submit"] $
        toHtml btnLabel

eventList :: [(Event, [Reply], Maybe Bool, Bool)] -> Bool -> Html ()
eventList events showAdmin = do
  div_ [class_ "container"] $ do
    div_ [class_ "row row-cols-1 g-3"] $ do
      div_ [class_ "col"] $ do
        when showAdmin $
          a_ [class_ "mb-2 btn btn-sm btn-primary", href_ "/veranstaltungen/neu", role_ "button"] "Neue Veranstaltung"
        h1_ [class_ "h3 m-0"] "Veranstaltungen"
      div_ [class_ "col"] $ do
        div_ [class_ "row row-cols-1 gy-4"] $ do
          div_ [class_ "col"] $ infoBox "Zum Öffnen einer Veranstaltung einfach auf den Titel klicken"
          forM_
            events
            ( \(event, replies, ownReply, isExpired) -> do
                let (id, title, date, familyAllowed, location, _) = event
                    (yesReplies, noReplies) = partition (unReplyComing . replyComing) replies
                    yes = fromIntegral (length yesReplies)
                    (no :: Integer) = fromIntegral (length noReplies)
                    guests = sum $ map (unReplyGuests . replyGuests) yesReplies
                    participantsTotal = yes + guests

                let dateFormatted = Text.pack . Time.formatTime german "%A, %d. %B %Y %R %p" $ (unEventDate date)

                    cardClass = case ownReply of
                      Just True -> "card border-success"
                      Just False -> "card border-danger"
                      _ -> "card"

                div_ [class_ "col"] $ do
                  div_ [class_ cardClass] $ do
                    div_ [class_ "card-header"] $ do
                      span_ [class_ "me-2"] $ toHtml dateFormatted
                      when isExpired $ span_ [class_ "badge bg-warning text-dark me-2"] "Bereits stattgefunden"
                      when familyAllowed $ span_ [class_ "badge bg-secondary me-2"] "Mit Familie"
                      case ownReply of
                        Just True -> span_ [class_ "badge bg-success text-white me-2"] "Zugesagt"
                        Just False -> span_ [class_ "badge bg-danger text-white"] "Abgesagt"
                        _ -> mempty
                    div_ [class_ "card-body"] $ do
                      h1_ [class_ "card-title h5"] $
                        a_ [href_ [i|/veranstaltungen/#{unEventID id}|]] $
                          toHtml $
                            unEventTitle title
                      h2_ [class_ "card-subtitle text-muted h6"] $ toHtml $ "Ort: " <> unEventLocation location
                      p_ [class_ "card-text"] ""
                      when (yes > 0 || no > 0) $
                        div_ [class_ "d-flex"] $ do
                          when (yes > 0) $
                            span_ [class_ "badge rounded-pill bg-success me-2"] $
                              toHtml $
                                ([i|Teilnehmer: #{participantsTotal}|] :: Text)
                          when (no > 0) $
                            span_ [class_ "badge rounded-pill bg-danger"] $
                              toHtml $
                                ([i|Absagen: #{no}|] :: Text)
            )

-- renderEvent displays a single event with all its information, including
-- attachments and who is coming, how many guests they are bringing, what the
-- loggined in user picked as RSVP. The event description is interpreted as
-- Markdown and the parser tries to recognize URLsl
renderEvent ::
  Event ->
  [Text] -> -- attachments
  [Reply] ->
  Maybe Reply ->
  Bool -> -- expired
  Bool -> -- show admin
  ReplyBox ->
  Html ()
renderEvent
  event
  attachments
  replies
  ownReply
  isExpired
  showAdmin
  whichReplyBoxToShow =
    let (eventid, title, date, familyAllowed, location, description) = event
        eventId = unEventID eventid

        (yesReplies, noReplies) = partition (unReplyComing . replyComing) replies
        yes = fromIntegral (length yesReplies)
        (no :: Integer) = fromIntegral (length noReplies)
        guests = sum $ map (unReplyGuests . replyGuests) yesReplies
        participantsTotal = yes + guests

        guestsYouAreBringing = case ownReply of
          Just Reply {..} ->
            if unReplyComing replyComing
              then unReplyGuests replyGuests
              else 0
          Nothing -> 0

        yourRsvp = case ownReply of
          Just Reply {..} -> Just $ unReplyComing replyComing
          Nothing -> Nothing
     in do
          let htmlEvent :: Html ()
              htmlEvent =
                do
                  let dateFormatted = Text.pack $ Time.formatTime german "%A, %d. %B %Y %R %p" $ unEventDate date
                  -- The actual event content, such as description and title
                  div_ [class_ $ "card" <> (if isExpired then " border-danger" else "")] $ do
                    div_ [class_ "card-header"] $ do
                      span_ [class_ "me-2"] $ toHtml dateFormatted
                      when isExpired $ span_ [class_ "badge bg-warning text-dark me-2"] "Bereits stattgefunden"
                      when familyAllowed $ span_ [class_ "badge bg-secondary"] "Mit Familie"
                    div_ [class_ "card-body"] $ do
                      h1_ [class_ "card-title h5"] $ toHtml $ unEventTitle title
                      h2_ [class_ "card-subtitle text-muted h6 mb-2"] $ toHtml $ "Ort: " <> unEventLocation location
                      p_ [class_ "card-text", style_ "white-space: pre-wrap"]
                        $ toHtmlRaw
                          . SanitizeXSS.sanitize
                          . CMarkGFM.commonmarkToHtml [] [CMarkGFM.extAutolink]
                        $ unEventDescription description
                      when (length attachments > 0) $ do
                        p_ [class_ "m-0"] "Angehängte Dateien: "
                        ul_ [class_ "m-0"] $ do
                          forM_
                            attachments
                            ( \filename ->
                                li_ [] $
                                  a_ [href_ [i|/events/#{eventId}/#{filename}|]] $
                                    toHtml filename
                            )
                    when showAdmin $ div_ [class_ "card-footer"] $ do
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

          let htmlRsvp :: Html ()
              htmlRsvp = do
                form_
                  [ class_ "my-2 g-3",
                    method_ "post",
                    action_ ("/veranstaltungen/" <> Text.pack (show eventId) <> "/antwort")
                  ]
                  $ do
                    div_ [class_ "row row-cols-1 gy-3"] $ do
                      div_ [class_ "col"] $ do
                        label_ [class_ "form-label", for_ "replySelect"] "Antwort"
                        select_
                          ( [ name_ "reply",
                              id_ "replySelect",
                              class_ "form-select form-select-sm me-1",
                              ariaLabel_ "Veranstaltung beantworten"
                            ]
                              ++ [disabled_ "disabled" | isExpired]
                          )
                          $ do
                            option_ (value_ "coming" : [selected_ "selected" | yourRsvp == Just True]) "Zusage"
                            option_ (value_ "notcoming" : [selected_ "selected" | yourRsvp == Just False]) "Absage"
                            option_ (value_ "noreply" : [selected_ "selected" | isNothing yourRsvp]) "Keine Antwort"
                      div_ [class_ "col"] $ do
                        label_ [class_ "form-label", for_ "numberOfGuests"] "Anzahl Gäste die du mitbringst"
                        input_
                          ( [ class_ "form-control form-control-sm",
                              type_ "number",
                              name_ "numberOfGuests",
                              id_ "numberOfGuests",
                              value_ [i|#{guestsYouAreBringing}|],
                              placeholder_ "0"
                            ]
                              ++ [disabled_ "disabled" | isExpired]
                          )
                      div_ [class_ "col"] $
                        button_
                          ( [type_ "submit", class_ "btn btn-primary btn-sm"]
                              ++ [disabled_ "disabled" | isExpired]
                          )
                          "Speichern"

          let htmlRsvpNoList :: Html ()
              htmlRsvpNoList =
                table_ [class_ "table"] $ do
                  thead_ $ do
                    tr_ $ do
                      th_ [scope_ "col"] "Email"
                      th_ [scope_ "col"] ""
                  tbody_ $ do
                    mapM_
                      ( \Reply {..} -> do
                          tr_ $ do
                            td_ [] $ toHtml $ show replyUserEmail
                            td_ [class_ "d-flex justify-content-end"] $
                              a_ [href_ . Text.pack $ "/nutzer/" <> show replyUserId] "Zum Profil"
                      )
                      noReplies

          let htmlRsvpYesList :: Html ()
              htmlRsvpYesList =
                table_ [class_ "table"] $ do
                  thead_ $ do
                    tr_ $ do
                      th_ [scope_ "col"] "Email"
                      th_ [scope_ "col"] "Gäste"
                      th_ [scope_ "col"] ""
                  tbody_ $ do
                    mapM_
                      ( \Reply {..} -> do
                          tr_ $ do
                            td_ [] $ toHtml $ show replyUserEmail
                            td_ [] $ toHtml $ show replyGuests
                            td_ [class_ "d-flex justify-content-end"] $
                              a_ [href_ . Text.pack $ "/nutzer/" <> show replyUserId] "Zum Profil"
                      )
                      yesReplies

          let checkSvg =
                toHtmlRaw
                  ( [i|
          <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-check-circle"><path d="M22 11.08V12a10 10 0 1 1-5.93-9.14"></path><polyline points="22 4 12 14.01 9 11.01"></polyline></svg>
          |] ::
                      Text
                  )

          let xCrossSvg =
                toHtmlRaw
                  ( [i|
          <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-x-circle"><circle cx="12" cy="12" r="10"></circle><line x1="15" y1="9" x2="9" y2="15"></line><line x1="9" y1="9" x2="15" y2="15"></line></svg>
          |] ::
                      Text
                  )

          div_ [class_ "container"] $ do
            div_ [class_ "row gy-3 gx-lg-4"] $ do
              section_ [class_ "col-lg-7"] $ htmlEvent
              section_ [class_ "col-lg-5"] $ do
                div_ [class_ "card border-primary"] $ do
                  div_ [class_ "card-header"] $ do
                    ul_ [class_ "nav nav-tabs card-header-tabs"] $ do
                      let makeHref box =
                            href_ $ decodeUtf8 $ URI.renderQuery True [("reply_box", Just box)]
                          makeClass boxType =
                            class_ $ Text.pack $ unwords $ ["nav-link"] ++ ["active" | whichReplyBoxToShow == boxType]
                          makeAttrs boxType box =
                            [makeClass boxType, makeHref box] ++ [ariaCurrent_ "true" | whichReplyBoxToShow == boxType]

                      a_ (makeAttrs Own "own") "Antwort"

                      a_ (makeAttrs Yes "yes") $ do
                        span_
                          [class_ "d-none d-md-block"]
                          [i|Teilnehmer (#{toHtml $ show participantsTotal})|]
                        span_ [class_ "d-md-none text-success"] $ do
                          checkSvg
                          [i|(#{toHtml $ show participantsTotal})|]

                      a_ (makeAttrs No "no") $ do
                        span_
                          [class_ "d-none d-md-block"]
                          [i|Absagen (#{toHtml $ show no})|]
                        span_ [class_ "d-md-none text-danger"] $ do
                          xCrossSvg
                          [i|(#{toHtml $ show no})|]

                  div_ [class_ "card-body"] $
                    case whichReplyBoxToShow of
                      Own -> htmlRsvp
                      Yes -> htmlRsvpYesList
                      No -> htmlRsvpNoList
