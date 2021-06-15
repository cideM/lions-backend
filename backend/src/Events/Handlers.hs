module Events.Handlers
  ( showAllEvents,
    showCreateEvent,
    showEvent,
    showDeleteEventConfirmation,
    handleDeleteEvent,
    handleCreateEvent,
    handleUpdateEvent,
    showEditEventForm,
    replyToEvent,
  )
where

import Control.Exception.Safe
import Control.Monad (when)
import Data.Foldable (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Events.DB
  ( createEvent,
    deleteEvent,
    deleteReply,
    getAll,
    getEvent,
    updateEvent,
    upsertReply,
  )
import Events.Domain (Event (..), EventCreate (..), EventId (..), Reply (..))
import qualified Events.EventForm as EventForm
import qualified Events.Preview
import qualified Events.SingleEvent
import GHC.Exts (sortWith)
import Layout (ActiveNavLink (..), LayoutStub (..))
import Locale (german)
import Lucid
import Network.HTTP.Types (status303)
import qualified Network.Wai as Wai
import qualified Session as Session
import Text.Read (readEither)
import User.DB (getUser)
import User.Types (UserProfile (..))
import Wai (parseParams)

showAllEvents ::
  SQLite.Connection ->
  Session.Authenticated ->
  IO LayoutStub
showAllEvents conn auth = do
  let (userIsAdmin, Session.UserSession {..}) = case auth of
        Session.IsAdmin (Session.AdminUser session) -> (True, session)
        Session.IsUser session -> (False, session)
  events <- map (addOwnReply userSessionUserId) . sortByDateDesc . Map.toList <$> getAll conn
  return . LayoutStub "Veranstaltungen" (Just Events) $
    div_ [class_ "container"] $ do
      when userIsAdmin $
        a_ [class_ "mb-1 mb-2 btn btn-sm btn-primary", href_ "/veranstaltungen/neu", role_ "button"] "Neue Veranstaltung"
      h1_ [class_ "h3 mb-3"] "Veranstaltungen"
      div_ [class_ "row row-cols-1 row-cols-lg-2 gy-4 gx-lg-4"] $
        mapM_ (div_ [class_ "col"] . Events.Preview.render) events
  where
    addOwnReply userid (eventid, e@Event {..}) = (eventid, e, find ((==) userid . replyUserId) eventReplies)
    sortByDateDesc = sortWith (Down . (\(_, Event {..}) -> eventDate))

replyToEvent ::
  SQLite.Connection ->
  Wai.Request ->
  (Wai.Response -> IO a) ->
  EventId ->
  Session.Authenticated ->
  IO a
replyToEvent conn req send eventid@(EventId i) auth = do
  let (_, Session.UserSession {..}) = case auth of
        Session.IsAdmin (Session.AdminUser session) -> (True, session)
        Session.IsUser session -> (False, session)
  UserProfile {..} <-
    getUser conn userSessionUserId >>= \case
      Nothing -> throwString $ "no user for userid from session, here's the user id: " <> show userSessionUserId
      Just v -> return v
  params <- parseParams req
  coming <-
    maybe (throwString "no 'reply' param") parseComing $
      Map.lookup "reply" params
  numberOfGuests <-
    maybe (throwString "no 'numberOfGuests' param") parseNumGuests $
      Map.lookup "numberOfGuests" params
  case coming of
    Nothing -> deleteReply conn eventid userSessionUserId
    Just yesno -> upsertReply conn eventid (Reply yesno userEmail userSessionUserId numberOfGuests)
  send $ Wai.responseLBS status303 [("Location", encodeUtf8 $ "/veranstaltungen/" <> Text.pack (show i))] mempty
  where
    parseComing "coming" = return $ Just True
    parseComing "notcoming" = return $ Just False
    parseComing "noreply" = return Nothing
    parseComing s = throwString . Text.unpack $ "unknown 'coming' value: " <> s

    parseNumGuests "" = return 0
    parseNumGuests s =
      case readEither (Text.unpack s) of
        Left e -> throwString . Text.unpack $ "couldn't parse '" <> s <> "' number of guests as number: " <> Text.pack (show e)
        Right i' -> return i'

showEvent ::
  SQLite.Connection ->
  EventId ->
  Session.Authenticated ->
  IO (Maybe LayoutStub)
showEvent conn eventid auth = do
  let (userIsAdmin, Session.UserSession {..}) = case auth of
        Session.IsAdmin (Session.AdminUser session) -> (True, session)
        Session.IsUser session -> (False, session)
  maybeevent <- (getEvent conn eventid)
  case maybeevent of
    Nothing -> return Nothing
    Just e@Event {..} -> do
      let ownReply = find ((==) userSessionUserId . replyUserId) eventReplies
      return . Just
        . LayoutStub
          eventTitle
          (Just Events)
        $ Events.SingleEvent.render (Events.SingleEvent.ShowAdminTools userIsAdmin) ownReply eventid e

showCreateEvent :: Session.AdminUser -> IO LayoutStub
showCreateEvent _ = do
  return . LayoutStub "Neue Veranstaltung" (Just Events) $
    div_ [class_ "container p-2"] $ do
      h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
      EventForm.render "Speichern" "/veranstaltungen/neu" EventForm.emptyForm EventForm.emptyState

handleDeleteEvent ::
  SQLite.Connection ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
handleDeleteEvent conn eventid _ = do
  maybeEvent <- getEvent conn eventid
  case maybeEvent of
    Nothing -> return . LayoutStub "Fehler" (Just Events) $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-secondary", role_ "alert"] "Kein Nutzer mit dieser ID gefunden"
    Just event -> do
      deleteEvent conn eventid
      return $
        LayoutStub "Veranstaltung" (Just Events) $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Veranstaltung " <> eventTitle event <> " erfolgreich gelöscht"

showDeleteEventConfirmation ::
  SQLite.Connection ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
showDeleteEventConfirmation conn eid@(EventId eventid) _ = do
  -- TODO: Duplicated
  getEvent conn eid >>= \case
    Nothing -> throwString $ "delete event but no event for id: " <> show eventid
    Just event -> do
      return . LayoutStub "Veranstaltung Löschen" (Just Events) $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-6"] $ do
            p_ [class_ "alert alert-danger mb-4", role_ "alert"] $
              toHtml ("Veranstaltung " <> eventTitle event <> " wirklich löschen?")
            form_
              [ action_ . Text.pack $ "/veranstaltungen/" <> show eventid <> "/loeschen",
                method_ "post",
                class_ "d-flex justify-content-center"
              ]
              $ button_ [class_ "btn btn-primary", type_ "submit"] "Ja, Veranstaltung löschen!"

handleCreateEvent ::
  SQLite.Connection ->
  Wai.Request ->
  Session.AdminUser ->
  IO LayoutStub
handleCreateEvent conn req _ = do
  params <- parseParams req
  let input =
        EventForm.FormInput
          (Map.findWithDefault "" "eventTitleInput" params)
          (Map.findWithDefault "" "eventDateInput" params)
          (Map.findWithDefault "" "eventLocationInput" params)
          (Map.findWithDefault "" "eventDescriptionInput" params)
          (isJust $ Map.lookup "eventFamAllowedInput" params)
  case EventForm.makeEvent input of
    Left state -> do
      return . LayoutStub "Neue Veranstaltung" (Just Events) $
        div_ [class_ "container p-2"] $ do
          h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
          EventForm.render "Speichern" "/veranstaltungen/neu" input state
    Right newEvent@EventCreate {..} -> do
      createEvent conn newEvent
      return . LayoutStub "Neue Veranstaltung" (Just Events) $
        div_ [class_ "container p-2"] $ do
          h1_ [class_ "h4 mb-3"] "Neue Veranstaltung erstellen"
          p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
            "Neue Veranstaltung " <> eventCreateTitle <> " erfolgreich erstellt!"

showEditEventForm ::
  SQLite.Connection ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
showEditEventForm conn eid@(EventId eventid) _ = do
  getEvent conn eid >>= \case
    Nothing -> throwString $ "edit event but no event for id: " <> show eventid
    Just Event {..} ->
      let input =
            EventForm.FormInput
              eventTitle
              (renderDateForInput eventDate)
              eventLocation
              eventDescription
              eventFamilyAllowed
       in return . LayoutStub "Veranstaltung Editieren" (Just Events) $
            div_ [class_ "container p-2"] $ do
              h1_ [class_ "h4 mb-3"] "Veranstaltung editieren"
              EventForm.render "Speichern" (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren") input EventForm.emptyState
  where
    renderDateForInput = Text.pack . Time.formatTime german "%d.%m.%Y %R"

handleUpdateEvent ::
  SQLite.Connection ->
  Wai.Request ->
  EventId ->
  Session.AdminUser ->
  IO LayoutStub
handleUpdateEvent conn req eid@(EventId eventid) _ = do
  params <- parseParams req
  let input =
        EventForm.FormInput
          (Map.findWithDefault "" "eventTitleInput" params)
          (Map.findWithDefault "" "eventDateInput" params)
          (Map.findWithDefault "" "eventLocationInput" params)
          (Map.findWithDefault "" "eventDescriptionInput" params)
          (isJust $ Map.lookup "eventFamAllowedInput" params)
  case EventForm.makeEvent input of
    Left state ->
      return $
        LayoutStub "Veranstaltung Editieren" (Just Events) $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            EventForm.render
              "Speichern"
              (Text.pack $ "/veranstaltungen/" <> show eventid <> "/editieren")
              input
              state
    Right event@EventCreate {..} -> do
      updateEvent conn eid event
      return $
        LayoutStub "Veranstaltung Editieren" (Just Events) $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            div_ [class_ "row col-6"] $ do
              p_ [class_ "alert alert-success", role_ "alert"] . toHtml $
                "Veranstaltung " <> eventCreateTitle <> " erfolgreich editiert"
