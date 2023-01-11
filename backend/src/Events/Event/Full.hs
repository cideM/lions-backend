module Events.Event.Full
  ( render,
    IsExpired (..),
    ShowAdminTools (..),
    ReplyBox (..),
  )
where

import Control.Monad (forM_, when)
import Data.Maybe (isNothing)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding as T
import qualified Data.Time as Time
import qualified Events.Event.Event as Events
import qualified Events.Event.Id as Event
import qualified Events.Reply.Reply as Event
import Layout (ariaCurrent_, ariaLabel_)
import Locale (german)
import Lucid
import qualified Network.HTTP.Types.URI as URI
import qualified User.Email as UserEmail
import qualified User.Id as User

newtype ShowAdminTools = ShowAdminTools Bool deriving (Show)

newtype IsExpired = IsExpired Bool deriving (Show)

renderEvent ::
  ShowAdminTools ->
  IsExpired ->
  Event.Id ->
  Events.Event ->
  Html ()
renderEvent
  (ShowAdminTools showAdminTools)
  (IsExpired isExpired)
  eid@(Event.Id eventId)
  Events.Event {..} = do
    let date = Text.pack . Time.formatTime german "%A, %d. %B %Y %R %p" $ eventDate
    -- The actual event content, such as description and title
    div_ [class_ $ "card" <> (if isExpired then " border-danger" else "")] $ do
      div_ [class_ "card-header"] $ do
        span_ [class_ "me-2"] $ toHtml date
        when isExpired $ span_ [class_ "badge bg-warning text-dark me-2"] "Bereits stattgefunden"
        when eventFamilyAllowed $ span_ [class_ "badge bg-secondary"] "Mit Familie"
      div_ [class_ "card-body"] $ do
        h1_ [class_ "card-title h5"] $ toHtml eventTitle
        h2_ [class_ "card-subtitle text-muted h6 mb-2"] $ toHtml $ "Ort: " <> eventLocation
        p_ [class_ "card-text", style_ "white-space: pre-wrap"] $ toHtml eventDescription
        when (length eventAttachments > 0) $ do
          p_ [class_ "m-0"] "Angehängte Dateien: "
          ul_ [class_ "m-0"] $ do
            forM_
              eventAttachments
              ( \filename ->
                  li_ [] $
                    a_ [href_ [i|/events/#{eventId}/#{filename}|]] $
                      toHtml filename
              )
      when showAdminTools $ div_ [class_ "card-footer"] $ renderAdminTools eid

renderAdminTools :: Event.Id -> Html ()
renderAdminTools (Event.Id eventId) = do
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

renderOwnForm :: IsExpired -> Maybe Event.Reply -> Event.Id -> Html ()
renderOwnForm (IsExpired isExpired) ownReply (Event.Id eventId) = do
  let replyGuests' = case replyComing' of
        Just True -> maybe mempty (Text.pack . show . Event.replyGuests) ownReply
        _ -> mempty
      replyComing' = fmap Event.replyComing ownReply
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
              option_ (value_ "coming" : [selected_ "selected" | replyComing' == Just True]) "Zusage"
              option_ (value_ "notcoming" : [selected_ "selected" | replyComing' == Just False]) "Absage"
              option_ (value_ "noreply" : [selected_ "selected" | isNothing replyComing']) "Keine Antwort"
        div_ [class_ "col"] $ do
          label_ [class_ "form-label", for_ "numberOfGuests"] "Anzahl Gäste die du mitbringst"
          input_
            ( [ class_ "form-control form-control-sm",
                type_ "number",
                name_ "numberOfGuests",
                id_ "numberOfGuests",
                value_ replyGuests',
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

renderNoList :: [Event.Reply] -> Html ()
renderNoList users =
  table_ [class_ "table"] $ do
    thead_ $ do
      tr_ $ do
        th_ [scope_ "col"] "Email"
        th_ [scope_ "col"] ""
    tbody_ $ do
      mapM_
        ( \Event.Reply {Event.replyUserEmail = UserEmail.Email email, Event.replyUserId = User.Id userid} -> do
            tr_ $ do
              td_ [] $ toHtml $ UserEmail.show email
              td_ [class_ "d-flex justify-content-end"] $
                a_ [href_ . Text.pack $ "/nutzer/" <> show userid] "Zum Profil"
        )
        users

renderYesList :: [Event.Reply] -> Html ()
renderYesList users =
  table_ [class_ "table"] $ do
    thead_ $ do
      tr_ $ do
        th_ [scope_ "col"] "Email"
        th_ [scope_ "col"] "Gäste"
        th_ [scope_ "col"] ""
    tbody_ $ do
      mapM_
        ( \Event.Reply {Event.replyUserEmail = UserEmail.Email email, Event.replyUserId = User.Id userid, ..} -> do
            tr_ $ do
              td_ [] $ toHtml $ UserEmail.show email
              td_ [] $ toHtml $ show replyGuests
              td_ [class_ "d-flex justify-content-end"] $
                a_ [href_ . Text.pack $ "/nutzer/" <> show userid] "Zum Profil"
        )
        users

renderReplies ::
  ReplyBox ->
  IsExpired ->
  Maybe Event.Reply ->
  Event.Id ->
  Events.Event ->
  Html ()
renderReplies activeBox expired ownReply eid Events.Event {..} = do
  let coming = length (filter Event.replyComing eventReplies)
      withGuests = coming + sum (map Event.replyGuests (filter Event.replyComing eventReplies))
      notComing = length (filter (not . Event.replyComing) eventReplies)
  div_ [class_ "card border-primary"] $ do
    div_ [class_ "card-header"] $ do
      ul_ [class_ "nav nav-tabs card-header-tabs"] $ do
        let makeHref box =
              href_ $ T.decodeUtf8 $ URI.renderQuery True [("reply_box", Just box)]
            makeClass boxType =
              class_ $ Text.pack $ unwords $ ["nav-link"] ++ ["active" | activeBox == boxType]
            makeAttrs boxType box =
              [makeClass boxType, makeHref box] ++ [ariaCurrent_ "true" | activeBox == boxType]

        a_ (makeAttrs Own "own") "Antwort"

        a_ (makeAttrs Yes "yes") $ do
          span_
            [class_ "d-none d-md-block"]
            [i|Teilnehmer (#{toHtml $ show withGuests})|]
          span_ [class_ "d-md-none text-success"] $ do
            checkSvg
            [i|(#{toHtml $ show withGuests})|]

        a_ (makeAttrs No "no") $ do
          span_
            [class_ "d-none d-md-block"]
            [i|Absagen (#{toHtml $ show notComing})|]
          span_ [class_ "d-md-none text-danger"] $ do
            xCrossSvg
            [i|(#{toHtml $ show notComing})|]

    div_ [class_ "card-body"] $
      case activeBox of
        Own -> renderOwnForm expired ownReply eid
        Yes -> renderYesList (filter Event.replyComing eventReplies)
        No -> renderNoList (filter (not . Event.replyComing) eventReplies)

data ReplyBox = Own | Yes | No deriving (Show, Eq)

render ::
  ReplyBox ->
  IsExpired ->
  ShowAdminTools ->
  Maybe Event.Reply ->
  Event.Id ->
  Events.Event ->
  Html ()
render activeBox expiration showAdminTools ownReply eid event =
  div_ [class_ "container"] $
    div_ [class_ "row gy-3 gx-lg-4"] $ do
      section_ [class_ "col-lg-7"] $ renderEvent showAdminTools expiration eid event

      section_ [class_ "col-lg-5"] $
        renderReplies activeBox expiration ownReply eid event

checkSvg :: Html ()
checkSvg =
  toHtmlRaw
    ( [i|
  <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-check-circle"><path d="M22 11.08V12a10 10 0 1 1-5.93-9.14"></path><polyline points="22 4 12 14.01 9 11.01"></polyline></svg>
  |] ::
        Text
    )

xCrossSvg :: Html ()
xCrossSvg =
  toHtmlRaw
    ( [i|
  <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-x-circle"><circle cx="12" cy="12" r="10"></circle><line x1="15" y1="9" x2="9" y2="15"></line><line x1="9" y1="9" x2="15" y2="15"></line></svg>
  |] ::
        Text
    )
