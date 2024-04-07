{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Activities.API where

import qualified App
import Control.Exception.Safe
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.QQ (sql)
import Form (FormFieldState (..))
import GHC.Generics
import qualified Katip as K
import Layout (LayoutStub (..), ariaCurrent_, describedBy_, success, warning)
import Locale (german)
import Lucid
import qualified Network.HTTP.Types.URI as URI
import qualified Network.Wai as Wai
import Text.Printf (printf)
import qualified UnliftIO
import qualified User.Id
import User.Session (Session (..))
import qualified User.Session
import Wai (parseParams)
import Prelude hiding (id)

data WorkTime = WorkTime
  { id :: Int,
    activityId :: Int,
    userId :: Int,
    userEmail :: Text,
    date :: Time.UTCTime,
    hours :: Int,
    minutes :: Int
  }
  deriving (Show)

data WorkTimeDbRow = WorkTimeDbRow
  { id :: Int,
    activityId :: Int,
    userId :: Int,
    userEmail :: Text,
    date :: Text,
    hours :: Int,
    minutes :: Int
  }
  deriving (FromRow, Generic, Show)

deleteWorkTime :: (MonadThrow m, MonadCatch m, MonadIO m) => SQLite.Connection -> Int -> m ()
deleteWorkTime conn workTimeId = do
  liftIO $ SQLite.execute conn [sql|delete from activity_times where id = ?|] (SQLite.Only workTimeId)

loadWorkTime :: (MonadThrow m, MonadCatch m, MonadIO m) => SQLite.Connection -> Int -> m (Maybe WorkTime)
loadWorkTime conn workTimeId = do
  rows <-
    liftIO $
      SQLite.query
        conn
        [sql|select activity_times.id, activity_id, user_id, users.email, date, hours, minutes
             from activity_times
             join users on activity_times.user_id = users.id
             where activity_times.id = ?|]
        (SQLite.Only workTimeId)
  case rows of
    [] -> return Nothing
    [WorkTimeDbRow {..}] -> do
      dateParsed <- case parseDateFromDb date of
        Nothing -> throwString $ "could not parse date: " <> Text.unpack date
        Just dateParsed -> return dateParsed
      return . Just $ WorkTime {date = dateParsed, ..}
    _ -> throwString "more than one row returned"

loadWorkTimes :: (MonadThrow m, MonadCatch m, MonadIO m) => SQLite.Connection -> Int -> m [WorkTime]
loadWorkTimes conn activityId_ = do
  rows <-
    liftIO $
      SQLite.query
        conn
        [sql|select activity_times.id, activity_id, user_id, users.email, date, hours, minutes
             from activity_times
             join users on activity_times.user_id = users.id
             where activity_id = ?|]
        (SQLite.Only activityId_)
  workTimes <- forM rows $ \WorkTimeDbRow {..} -> do
    dateParsed <- case parseDateFromDb date of
      Nothing -> throwString $ "could not parse date: " <> Text.unpack date
      Just dateParsed -> return dateParsed
    return WorkTime {date = dateParsed, ..}
  return workTimes

processWorkTimes :: Int -> [WorkTime] -> ([WorkTime], Map (Int, Text) [WorkTime])
processWorkTimes userIdInt workTimes = do
  let ownTimes = sortByDateDesc $ filter (\WorkTime {userId} -> userId == userIdInt) workTimes
      otherTimesGroupedByUser =
        Map.fromListWith (<>)
          . map (\t@WorkTime {userId, userEmail} -> ((userId, userEmail), [t]))
          $ filter (\WorkTime {userId} -> userId /= userIdInt) workTimes
  (ownTimes, otherTimesGroupedByUser)

data Activity = Activity
  { id :: Int,
    name :: Text,
    description :: Maybe Text,
    location :: Maybe Text,
    date :: Maybe Time.UTCTime
  }
  deriving (Show)

data ActivityDbRow = ActivityDbRow
  { id :: Int,
    name :: Text,
    description :: Maybe Text,
    location :: Maybe Text,
    date :: Maybe Text
  }
  deriving (FromRow, Generic, Show)

parseDateFromDb :: Text -> Maybe Time.UTCTime
parseDateFromDb date = Time.parseTimeM True german "%Y-%m-%d %T" (Text.unpack date)

delete :: (MonadThrow m, MonadCatch m, MonadIO m) => SQLite.Connection -> Int -> m ()
delete conn activityId = do
  liftIO $ SQLite.execute conn [sql|delete from activities where id = ?|] (SQLite.Only activityId)

load :: (MonadThrow m, MonadCatch m, MonadIO m) => SQLite.Connection -> Int -> m (Maybe Activity)
load conn activityId = do
  rows <- liftIO $ SQLite.query conn [sql|select id, name, description, location, date from activities where id = ?|] (SQLite.Only activityId)
  case rows of
    [] -> return Nothing
    [ActivityDbRow {..}] -> do
      dateParsed <- case date of
        Nothing -> return Nothing
        Just justDate -> case parseDateFromDb justDate of
          Nothing -> throwString $ "could not parse date: " <> Text.unpack justDate
          Just dateParsed -> return $ Just dateParsed
      return . Just $ Activity {date = dateParsed, ..}
    _ -> throwString "more than one row returned"

loadAll :: (MonadThrow m, MonadCatch m, MonadIO m) => SQLite.Connection -> m [Activity]
loadAll conn = do
  rows <- liftIO $ SQLite.query_ conn [sql|select id, name, description, location, date from activities|]
  activities <- forM rows $ \ActivityDbRow {..} -> do
    dateParsed <- case date of
      Nothing -> return Nothing
      Just justDate -> case parseDateFromDb justDate of
        Nothing -> throwString $ "could not parse date: " <> Text.unpack justDate
        Just dateParsed -> return $ Just dateParsed
    return Activity {date = dateParsed, ..}
  return activities

minutesToHoursAndMinutes :: Int -> (Int, Int)
minutesToHoursAndMinutes minutes = (minutes `div` 60, minutes `mod` 60)

data RenderActivityTabs = AddTime | ShowTimes | ShowSummary
  deriving (Eq)

renderActivity ::
  Bool ->
  RenderActivityTabs ->
  InputAddTimeForm ->
  [WorkTime] ->
  Map (Int, Text) [WorkTime] ->
  Activity ->
  Html ()
renderActivity canEdit activeTab input ownTimes otherTimes activity@Activity {..} = do
  div_ [class_ "container-lg"] $ do
    div_ [class_ "row"] $ do
      div_ [class_ "col-12 col-md-5"] $ do
        article_ [class_ "d-flex flex-column gap-3"] $ do
          header_ [class_ "d-flex flex-column gap-0"] $ do
            when canEdit $
              div_ $
                a_ [href_ [i|/activities/#{id}/edit|], class_ "btn btn-sm btn-secondary", role_ "button"] "Bearbeiten"
            h1_ [class_ "m-0"] (toHtml name)
            case date of
              Nothing -> mempty
              Just justDate ->
                p_ [class_ "text-body-secondary m-0"] (toHtml $ formatDate justDate)
            case location of
              Nothing -> mempty
              Just justLocation ->
                p_ [class_ "text-body-secondary m-0"] (toHtml $ justLocation)
          div_ $ do
            p_ (toHtml $ fromMaybe "" description)
      div_ [class_ "col-12 col-md-7"] $ do
        div_ [class_ "card border-primary mb-3"] $ do
          div_ [class_ "card-header"] $ do
            ul_ [class_ "nav nav-tabs card-header-tabs"] $ do
              li_ [class_ "nav-item"] $
                a_
                  [ class_ $ "nav-link" <> if activeTab == AddTime then " active" else "",
                    href_ . decodeUtf8 $
                      URI.renderQuery
                        True
                        [("active_tab", Just "add_time")],
                    ariaCurrent_ $ if activeTab == AddTime then "true" else "false"
                  ]
                  "Zeit hinzufügen"
              li_ [class_ "nav-item"] $
                a_
                  [ class_ $ "nav-link" <> if activeTab == ShowTimes then " active" else "",
                    href_ . decodeUtf8 $
                      URI.renderQuery
                        True
                        [("active_tab", Just "show_times")],
                    ariaCurrent_ $ if activeTab == ShowTimes then "true" else "false"
                  ]
                  "Details"
              li_ [class_ "nav-item"] $
                a_
                  [ class_ $ "nav-link" <> if activeTab == ShowSummary then " active" else "",
                    href_ . decodeUtf8 $
                      URI.renderQuery
                        True
                        [("active_tab", Just "show_summary")],
                    ariaCurrent_ $ if activeTab == ShowSummary then "true" else "false"
                  ]
                  "Zusammenfassung"
          div_ [class_ "card-body"] $ do
            case activeTab of
              AddTime -> do
                when (length ownTimes > 0) $ do
                  let totalTime = foldr (\WorkTime {hours, minutes} (h, m) -> (h + hours, m + minutes)) (0, 0) ownTimes
                      (totalHours, totalMinutes) = minutesToHoursAndMinutes (fst totalTime * 60 + snd totalTime)
                  p_ [class_ "alert alert-success"] [i|Du hast bereits #{totalHours} Stunden und #{totalMinutes} Minuten eingetragen.|]
                form_ [class_ "d-flex flex-column gap-2", action_ [i|/activities/#{id}|], method_ "post"] $ do
                  div_ $ do
                    label_ [for_ "date", class_ "form-label"] "Wann die Arbeit geleistet wurde"
                    input_
                      [ type_ "date",
                        class_ $
                          "form-control" <> case input.date.state of
                            Valid _ -> " is-valid"
                            Invalid _ -> " is-invalid"
                            _ -> "",
                        id_ "date",
                        name_ "date",
                        value_ input.date.value,
                        required_ "required"
                      ]
                    case input.date.state of
                      Invalid msg -> label_ [for_ "date", class_ "invalid-feedback"] (toHtml msg)
                      _ -> mempty
                  div_ $ do
                    label_ [for_ "hours", class_ "form-label"] "Stunden"
                    input_
                      [ type_ "number",
                        class_ $
                          "form-control" <> case input.hours.state of
                            Valid _ -> " is-valid"
                            Invalid _ -> " is-invalid"
                            _ -> "",
                        id_ "hours",
                        name_ "hours",
                        value_ input.hours.value,
                        required_ "required"
                      ]
                    case input.hours.state of
                      Invalid msg -> label_ [for_ "hours", class_ "invalid-feedback"] (toHtml msg)
                      _ -> mempty
                  div_ $ do
                    label_ [for_ "minutes", class_ "form-label"] "Minuten"
                    input_
                      [ type_ "number",
                        class_ $
                          "form-control" <> case input.minutes.state of
                            Valid _ -> " is-valid"
                            Invalid _ -> " is-invalid"
                            _ -> "",
                        id_ "minutes",
                        name_ "minutes",
                        value_ input.minutes.value,
                        required_ "required"
                      ]
                    case input.minutes.state of
                      Invalid msg -> label_ [for_ "minutes", class_ "invalid-feedback"] (toHtml msg)
                      _ -> mempty
                  button_ [class_ "btn btn-primary", type_ "submit"] "Speichern"
              ShowTimes -> do
                div_ [class_ "d-flex flex-column"] $ do
                  h2_ [class_ "h5"] "Eigene Zeiten"
                  table_ [class_ "table table-sm"] $ do
                    thead_ $ do
                      tr_ $ do
                        th_ [scope_ "col"] "Datum"
                        th_ [scope_ "col"] "hh:mm"
                        th_ [scope_ "col"] ""
                    forM_ ownTimes $ \wt -> do
                      let activityId = activity.id
                          worktimeId = wt.id
                      tbody_ $ do
                        tr_ $ do
                          td_ (toHtml . Text.pack $ Time.formatTime german "%d.%m.%Y" wt.date)
                          td_ $ toHtml (printf "%02d:%02d" wt.hours wt.minutes :: String)
                          td_ $ do
                            a_
                              [ href_ [i|/activities/#{activityId}/times/#{worktimeId}/delete|],
                                class_ "btn btn-sm btn-danger",
                                role_ "button"
                              ]
                              "Löschen"
                div_ [class_ "d-flex flex-column"] $ do
                  h2_ [class_ "h5"] "Zeiten anderer"
                  forM_ (Map.toList otherTimes) $ \((userId, userEmail), times) -> do
                    div_ [class_ "d-flex gap-2"] $ do
                      h3_ [class_ "h6 m-0"] $ do
                        a_ [href_ [i|/nutzer/#{userId}|]] (toHtml userEmail)
                    table_ [class_ "table table-sm"] $ do
                      thead_ $ do
                        tr_ $ do
                          th_ [scope_ "col"] "Datum"
                          th_ [scope_ "col"] "hh:mm"
                      tbody_ $ do
                        forM_ times $ \wt -> do
                          tr_ $ do
                            td_ (toHtml . Text.pack $ Time.formatTime german "%d.%m.%Y" wt.date)
                            td_ $ toHtml (printf "%02d:%02d" wt.hours wt.minutes :: String)
              ShowSummary -> do
                let otherTimesAsList :: [WorkTime] = concatMap snd $ Map.toList otherTimes
                    allTimes :: [WorkTime] = ownTimes <> otherTimesAsList
                    totalTimeInMinutes = foldr (\WorkTime {hours, minutes} acc -> acc + hours * 60 + minutes) 0 allTimes
                    (totalHours, totalMinutes) = minutesToHoursAndMinutes totalTimeInMinutes
                    numberOfPeople = length allTimes
                div_ [class_ "d-flex flex-column"] $ do
                  p_ [i|Insgesamt wurden #{totalHours} Stunden und #{totalMinutes} Minuten von #{numberOfPeople} Personen eingetragen.|]

renderAll :: Bool -> [Activity] -> Html ()
renderAll canCreate activities = do
  main_ [class_ "container-lg d-flex flex-column gap-3"] $ do
    div_ [class_ "d-flex flex-column gap-1"] $ do
      when canCreate $
        div_ $
          a_ [href_ "/activities/create", class_ "btn btn-sm btn-primary", role_ "button"] "Neue Activity"
      h1_ [class_ "h3 m-0"] "Activities"
    div_ [class_ "d-flex flex-column gap-3"] $ do
      forM_ activities $ \Activity {..} -> do
        article_ [class_ "card"] $ do
          case date of
            Nothing -> mempty
            Just justDate -> do
              p_ [class_ "card-header"] (toHtml $ formatDate justDate)
          div_ [class_ "card-body"] $ do
            h2_ [class_ "card-title h4"] (toHtml name)
            case location of
              Nothing -> mempty
              Just justLocation -> do
                p_ [class_ "card-subtitle text-body-secondary mb-2"] (toHtml justLocation)
            case description of
              Nothing -> mempty
              Just justDesc ->
                p_ (toHtml justDesc)
          div_ [class_ "card-footer d-flex gap-2 justify-content-between"] $ do
            when canCreate $ do
              div_ [class_ "d-flex gap-2"] $ do
                a_ [href_ [i|/activities/#{id}/delete|], class_ "btn btn-sm btn-danger", role_ "button"] "Löschen"
                a_ [href_ [i|/activities/#{id}/edit|], class_ "btn btn-sm btn-secondary", role_ "button"] "Bearbeiten"
              a_ [href_ [i|/activities/#{id}|], class_ "btn btn-sm btn-primary align-self-end", role_ "button"] "Öffnen"

get ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    MonadCatch m,
    K.KatipContext m,
    App.HasDb env
  ) =>
  Int ->
  Wai.Request ->
  User.Session.Authenticated ->
  m LayoutStub
get activityId request auth = do
  let Session {sessionUserId} = User.Session.get' auth
      userIdInt = User.Id.unId sessionUserId
  K.katipAddNamespace "activities.get" $ do
    K.katipAddContext (K.sl "activity_id" activityId) $ do
      conn <- asks App.getDb
      maybeActivity <- load conn activityId
      case maybeActivity of
        Nothing -> return . LayoutStub "Activity nicht gefunden" $ warning "Activity nicht gefunden"
        Just activity@Activity {..} -> do
          let currentUserIsAdmin = User.Session.isAdmin' auth
          let input =
                InputAddTimeForm
                  { date = Field {value = "", state = NotValidated},
                    hours = Field {value = "0", state = NotValidated},
                    minutes = Field {value = "0", state = NotValidated}
                  }
          (ownTimes, otherTimes) <- processWorkTimes userIdInt <$> loadWorkTimes conn activityId
          return . LayoutStub name $
            renderActivity
              currentUserIsAdmin
              (getActiveTabFromRequest request)
              input
              ownTimes
              otherTimes
              activity

getAll ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    MonadCatch m,
    K.KatipContext m,
    App.HasDb env
  ) =>
  User.Session.Authenticated ->
  m LayoutStub
getAll auth = do
  K.katipAddNamespace "activities.getAll" $ do
    conn <- asks App.getDb
    K.logLocM K.DebugS "loading all activities"
    activities <- loadAll conn

    let currentUserIsAdmin = User.Session.isAdmin' auth

    return (LayoutStub "Activities" (renderAll currentUserIsAdmin activities))

data FormMode = Create | Update Int

parseDateFormInput :: Text -> Maybe Time.UTCTime
parseDateFormInput date = Time.parseTimeM True german "%d.%m.%Y %R" (Text.unpack date)

formatDate :: Time.UTCTime -> Text
formatDate = Text.pack . Time.formatTime german "%A, %d. %B %Y %R %p"

formatDateInput :: Time.UTCTime -> Text
formatDateInput = Text.pack . Time.formatTime german "%d.%m.%Y %R"

data Field input parsed = Field
  { value :: input,
    state :: FormFieldState parsed
  }

data InputAddTimeForm = InputAddTimeForm
  { date :: Field Text Time.UTCTime,
    hours :: Field Text Int,
    minutes :: Field Text Int
  }

parseInputAddTimeForm :: Map.Map Text Text -> InputAddTimeForm
parseInputAddTimeForm params =
  let date = fromMaybe "" $ Map.lookup "date" params
      hours = fromMaybe "" $ Map.lookup "hours" params
      minutes = fromMaybe "" $ Map.lookup "minutes" params
      dateField =
        Field
          { value = date,
            state = case date of
              "" -> Invalid "Datum darf nicht leer sein"
              date_ -> case (Time.parseTimeM True german "%Y-%m-%d" $ Text.unpack date_) of
                Nothing -> Invalid "Datum ungültig"
                Just parsedDate -> Valid parsedDate
          }
      hoursField =
        Field
          { value = hours,
            state = case hours of
              "" -> Invalid "Stunden dürfen nicht leer sein"
              hours_ -> case reads (Text.unpack hours_) of
                [(hoursParsed, "")] -> if hoursParsed >= 0 then Valid hoursParsed else Invalid "Stunden müssen positiv sein"
                _ -> Invalid "Stunden müssen eine ganze Zahl sein"
          }
      minutesField =
        Field
          { value = minutes,
            state = case minutes of
              "" -> Invalid "Minuten dürfen nicht leer sein"
              minutes_ -> case reads (Text.unpack minutes_) of
                [(minutesParsed, "")] -> if minutesParsed >= 0 then Valid minutesParsed else Invalid "Minuten müssen positiv sein"
                _ -> Invalid "Minuten müssen eine ganze Zahl sein"
          }
   in InputAddTimeForm {date = dateField, hours = hoursField, minutes = minutesField}

data InputActivityForm = InputActivityForm
  { name :: Field Text Text,
    description :: Field (Maybe Text) (Maybe Text),
    location :: Field (Maybe Text) (Maybe Text),
    date :: Field (Maybe Text) (Maybe Time.UTCTime)
  }

parseInputActivityForm :: Map.Map Text Text -> InputActivityForm
parseInputActivityForm params =
  let title = fromMaybe "" $ Map.lookup "name" params
      description = Map.lookup "description" params
      location = fromMaybe "" $ Map.lookup "location" params
      date = fromMaybe "" $ Map.lookup "date" params
      titleField =
        Field
          { value = title,
            state = case title of
              "" -> Invalid "Titel darf nicht leer sein"
              _ -> Valid title
          }
      locationField = Field {value = (Just location), state = Valid (Just location)}
      descriptionField = Field {value = description, state = Valid description}
      dateField =
        Field
          { value = (Just date),
            state = case date of
              "" -> Valid Nothing
              date_ -> case parseDateFormInput date_ of
                Nothing -> Invalid "Datum ungültig"
                Just parsedDate -> Valid (Just parsedDate)
          }
   in InputActivityForm {name = titleField, description = descriptionField, location = locationField, date = dateField}

renderForm ::
  FormMode ->
  InputActivityForm ->
  Html ()
renderForm mode input = do
  let (formTitle :: Text) = case mode of
        Create -> "Neue Activity"
        Update _ -> "Activity bearbeiten"
      formAction = case mode of
        Create -> "/activities/create"
        Update id -> [i|/activities/#{id}/edit|]

  main_ [class_ "container-lg d-flex flex-column gap-3"] $ do
    h1_ [class_ "h3 m-0"] (toHtml formTitle)
    form_ [action_ formAction, method_ "post", class_ "d-flex flex-column gap-4"] $ do
      div_ [class_ "d-flex flex-column"] $ do
        label_ [for_ "name", class_ "form-label"] "Name"
        input_
          [ type_ "text",
            class_ $
              "form-control" <> case input.name.state of
                Valid _ -> " is-valid"
                Invalid _ -> " is-invalid"
                _ -> "",
            id_ "name",
            name_ "name",
            required_ "required",
            value_ input.name.value
          ]
        case input.name.state of
          Invalid msg -> label_ [for_ "name", class_ "invalid-feedback"] (toHtml msg)
          _ -> mempty
      div_ [class_ "d-flex flex-column"] $ do
        label_ [for_ "description", class_ "form-label"] "Beschreibung"
        textarea_
          [ class_ $ "form-control",
            id_ "description",
            name_ "description",
            value_ (fromMaybe "" input.description.value)
          ]
          ""
      div_ [class_ "d-flex flex-column"] $ do
        label_ [for_ "location", class_ "form-label"] "Ort"
        input_
          [ type_ "text",
            class_ $
              "form-control" <> case input.location.state of
                Valid _ -> " is-valid"
                Invalid _ -> " is-invalid"
                _ -> "",
            id_ "location",
            name_ "location",
            value_ $ fromMaybe "" input.location.value
          ]
        case input.location.state of
          Invalid msg -> label_ [for_ "location", class_ "invalid-feedback"] (toHtml msg)
          _ -> mempty
      div_ [class_ "d-flex flex-column"] $ do
        label_ [for_ "date", class_ "form-label"] "Datum"
        input_
          [ type_ "text",
            class_ $
              "form-control" <> case input.date.state of
                Valid _ -> " is-valid"
                Invalid _ -> " is-invalid"
                _ -> "",
            pattern_ "\\d{2}.\\d{2}.\\d{4} \\d{2}:\\d{2}",
            id_ "date",
            name_ "date",
            value_ $ fromMaybe "" input.date.value,
            describedBy_ "dateDescription"
          ]
        label_ [for_ "date", class_ "form-text"] "Bitte als Format '12.01.2022 15:00' verwenden."
        case input.date.state of
          Invalid msg -> label_ [for_ "date", class_ "invalid-feedback"] (toHtml msg)
          _ -> mempty
      button_ [type_ "submit", class_ "btn btn-primary"] "Speichern"

postCreate ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    K.KatipContext m,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env
  ) =>
  Wai.Request ->
  User.Session.Admin ->
  m LayoutStub
postCreate request _ = do
  K.katipAddNamespace "activities.postCreate" $ do
    input <- liftIO $ parseInputActivityForm <$> parseParams request

    let context =
          ( K.sl "title" input.name.value
              <> K.sl "description" input.description.value
              <> K.sl "location" input.location.value
              <> K.sl "date" input.date.value
          )
        pageTitle = "Neue Activity"

    K.katipAddContext context $ case input of
      InputActivityForm
        { name = Field {state = Valid name},
          description = Field {state = Valid description},
          location = Field {state = Valid location},
          date = Field {state = Valid date}
        } -> do
          K.logLocM K.DebugS "form valid"
          conn <- asks App.getDb
          liftIO $
            SQLite.execute
              conn
              "insert into activities (name, description, location, date) values (?, ?, ?, ?)"
              (name, description, location, date)
          rowId <- liftIO $ SQLite.lastInsertRowId conn
          K.logLocM K.DebugS $ "inserted row with id: " <> K.logStr (show rowId)
          -- TODO: redirect
          return . LayoutStub pageTitle $ success "Activity gespeichert"
      _ -> do
        K.logLocM K.DebugS "form invalid"
        return . LayoutStub pageTitle $ renderForm Create input

getCreate ::
  ( MonadIO m,
    MonadReader env m,
    K.KatipContext m
  ) =>
  User.Session.Admin ->
  m LayoutStub
getCreate _ = do
  K.katipAddNamespace "activities.getCreate" $ do
    let titleField = Field {value = "", state = NotValidated}
        descriptionField = Field {value = Nothing, state = Valid Nothing}
        locationField = Field {value = Nothing, state = NotValidated}
        dateField = Field {value = Nothing, state = NotValidated}
        input = InputActivityForm {name = titleField, description = descriptionField, location = locationField, date = dateField}
    return . LayoutStub "Neue Activity" $ renderForm Create input

getEdit ::
  ( MonadIO m,
    MonadReader env m,
    MonadCatch m,
    App.HasDb env,
    K.KatipContext m
  ) =>
  Int ->
  User.Session.Admin ->
  m LayoutStub
getEdit activityId _ = do
  K.katipAddNamespace "activities.getEdit" $ do
    conn <- asks App.getDb

    maybeActivity <- load conn activityId
    case maybeActivity of
      Nothing -> throwString "Activity nicht gefunden"
      Just Activity {..} -> do
        let titleField = Field {value = name, state = Valid name}
            descriptionField = Field {value = description, state = NotValidated}
            locationField = Field {value = location, state = NotValidated}
            dateField = Field {value = (formatDateInput <$> date), state = NotValidated}
            input =
              InputActivityForm
                { name = titleField,
                  description = descriptionField,
                  location = locationField,
                  date = dateField
                }
        return . LayoutStub "Activity bearbeiten" $ renderForm (Update activityId) input

postEdit ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    K.KatipContext m,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env
  ) =>
  Int ->
  Wai.Request ->
  User.Session.Admin ->
  m LayoutStub
postEdit activityId request _ = do
  K.katipAddNamespace "activities.postEdit" $ do
    input <- liftIO $ parseInputActivityForm <$> parseParams request

    let context =
          ( K.sl "title" input.name.value
              <> K.sl "description" input.description.value
              <> K.sl "location" input.location.value
              <> K.sl "date" input.date.value
          )
        pageTitle = "Activity bearbeiten"

    K.katipAddContext context $ case input of
      InputActivityForm
        { name = Field {state = Valid name},
          description = Field {state = Valid description},
          location = Field {state = Valid location},
          date = Field {state = Valid date}
        } -> do
          K.logLocM K.DebugS "form valid"
          conn <- asks App.getDb
          liftIO $
            SQLite.execute
              conn
              "update activities set name = ?, description = ?, location = ?, date = ? where id = ?"
              (name, description, location, date, activityId)
          return . LayoutStub pageTitle $ success "Activity gespeichert"
      _ -> do
        K.logLocM K.DebugS "form invalid"
        return . LayoutStub pageTitle $ renderForm Create input

getConfirmDelete ::
  ( MonadIO m,
    App.HasDb env,
    K.KatipContext m,
    MonadReader env m,
    MonadCatch m,
    MonadThrow m
  ) =>
  Int ->
  User.Session.Admin ->
  m LayoutStub
getConfirmDelete activityId _ = do
  K.katipAddNamespace "activities.getConfirmDelete" $ do
    conn <- asks App.getDb

    maybeActivity <- load conn activityId
    case maybeActivity of
      Nothing -> throwString "Activity nicht gefunden"
      Just Activity {..} -> do
        return . LayoutStub "Activity Löschen" $
          main_ [class_ "container-lg d-flex flex-column gap-3"] $ do
            h1_ [class_ "h3 m-0"] [i|Activity #{name} löschen?|]
            form_ [action_ [i|/activities/#{activityId}/delete|], method_ "post"] $ do
              button_ [class_ "btn btn-primary btn-danger", type_ "submit"] "Löschen"

postDelete ::
  ( MonadIO m,
    App.HasDb env,
    K.KatipContext m,
    MonadReader env m,
    MonadCatch m,
    MonadThrow m
  ) =>
  Int ->
  User.Session.Admin ->
  m LayoutStub
postDelete activityId _ = do
  K.katipAddNamespace "activities.postDelete" $ do
    conn <- asks App.getDb

    maybeActivity <- load conn activityId
    case maybeActivity of
      Nothing -> throwString "Activity nicht gefunden"
      Just Activity {..} -> do
        delete conn activityId
        return . LayoutStub "Activity Löschen" $
          main_ [class_ "container-lg d-flex flex-column gap-3"] $ do
            p_ [class_ "alert alert-success", role_ "alert"] [i|Activity #{name} gelöscht|]
            a_ [href_ "/activities"] "Zurück"

saveTime ::
  (MonadThrow m, MonadCatch m, MonadIO m) =>
  SQLite.Connection ->
  Int ->
  Int ->
  Time.UTCTime ->
  Int ->
  Int ->
  m ()
saveTime conn activityId userId date hours minutes = do
  liftIO $ SQLite.execute conn "insert into activity_times (activity_id, user_id, date, hours, minutes) values (?, ?, ?, ?, ?)" (activityId, userId, date, hours, minutes)
  return ()

sortByDateDesc :: [WorkTime] -> [WorkTime]
sortByDateDesc = reverse . sortOn (\WorkTime {..} -> date)

getActiveTabFromRequest :: Wai.Request -> RenderActivityTabs
getActiveTabFromRequest request =
  case Wai.queryString request of
    [("active_tab", Just "show_times")] -> ShowTimes
    [("active_tab", Just "show_summary")] -> ShowSummary
    _ -> AddTime

postAddTime ::
  ( MonadIO m,
    App.HasDb env,
    UnliftIO.MonadUnliftIO m,
    K.KatipContext m,
    MonadReader env m,
    MonadCatch m,
    MonadThrow m
  ) =>
  Int ->
  Wai.Request ->
  User.Session.Authenticated ->
  m LayoutStub
postAddTime activityId request auth = do
  K.katipAddNamespace "activities.postAddTime" $ do
    let Session {sessionUserId} = User.Session.get' auth
        userIdInt = User.Id.unId sessionUserId
        currentUserIsAdmin = User.Session.isAdmin' auth

    input <- liftIO $ parseInputAddTimeForm <$> parseParams request

    let context =
          ( K.sl "date" (input.date.value)
              <> K.sl "hours" (input.hours.value)
              <> K.sl "minutes" (input.minutes.value)
              <> K.sl "activity_id" activityId
              <> K.sl "user_id" userIdInt
          )

    K.katipAddContext context $ do
      K.logLocM K.DebugS "parsed form, saving time"
      conn <- asks App.getDb
      UnliftIO.withRunInIO $ \runInIO -> do
        SQLite.withTransaction conn . runInIO $ do
          maybeActivity <- load conn activityId
          case maybeActivity of
            Nothing -> throwString "Activity nicht gefunden"
            Just activity -> do
              case (input.date.state, input.hours.state, input.minutes.state) of
                (Valid date, Valid hours, Valid minutes) -> do
                  _ <- saveTime conn activityId userIdInt date hours minutes
                  (ownTimes, otherTimes) <- processWorkTimes userIdInt <$> loadWorkTimes conn activityId
                  let emptyInput =
                        InputAddTimeForm
                          { date = Field {value = "", state = NotValidated},
                            hours = Field {value = "0", state = NotValidated},
                            minutes = Field {value = "0", state = NotValidated}
                          }

                  return . LayoutStub activity.name $
                    renderActivity
                      currentUserIsAdmin
                      AddTime
                      emptyInput
                      ownTimes
                      otherTimes
                      activity
                _ -> do
                  K.logLocM K.DebugS "form invalid"
                  (ownTimes, otherTimes) <- processWorkTimes userIdInt <$> loadWorkTimes conn activityId
                  return . LayoutStub activity.name $
                    renderActivity
                      currentUserIsAdmin
                      AddTime
                      input
                      ownTimes
                      otherTimes
                      activity

getConfirmDeleteTime ::
  ( MonadIO m,
    App.HasDb env,
    K.KatipContext m,
    MonadReader env m,
    MonadCatch m,
    MonadThrow m
  ) =>
  Int ->
  User.Session.Authenticated ->
  m LayoutStub
getConfirmDeleteTime timeId auth = do
  K.katipAddNamespace "activities.getConfirmDeleteTime" $ do
    let Session {sessionUserId} = User.Session.get' auth
        userIdInt = User.Id.unId sessionUserId

    conn <- asks App.getDb
    maybeWorkTime <- loadWorkTime conn timeId
    case maybeWorkTime of
      Nothing -> throwString "Zeit nicht gefunden"
      Just WorkTime {..} -> do
        if userIdInt /= userId
          then throwString "Nicht erlaubt"
          else do
            return . LayoutStub "Zeit löschen" $ do
              div_ [class_ "container-lg d-flex flex-column gap-3"] $ do
                p_ [class_ "alert alert-danger", role_ "alert"] [i|Soll die Zeit vom #{Time.formatTime german "%d.%m.%Y" date} wirklich gelöscht werden?|]
                form_ [class_ "d-flex flex-row justify-content-end gap-3", action_ [i|/activities/#{activityId}/times/#{timeId}/delete|], method_ "post"] $ do
                  button_ [class_ "btn btn-danger", type_ "submit"] "Löschen"
                  a_ [href_ [i|/activities/#{activityId}|], class_ "btn btn-secondary", role_ "button"] "Abbrechen"

postDeleteTime ::
  ( MonadIO m,
    App.HasDb env,
    K.KatipContext m,
    MonadReader env m,
    MonadCatch m,
    MonadThrow m
  ) =>
  Int ->
  User.Session.Authenticated ->
  m LayoutStub
postDeleteTime timeId auth = do
  K.katipAddNamespace "activities.postDeleteTime" $ do
    let Session {sessionUserId} = User.Session.get' auth
        userIdInt = User.Id.unId sessionUserId

    conn <- asks App.getDb
    maybeWorkTime <- loadWorkTime conn timeId
    case maybeWorkTime of
      Nothing -> throwString "Zeit nicht gefunden"
      Just WorkTime {..} -> do
        if userIdInt /= userId
          then throwString "Nicht erlaubt"
          else do
            deleteWorkTime conn timeId
            return . LayoutStub "Zeit löschen" $ do
              div_ [class_ "container-lg d-flex flex-column gap-3"] $ do
                p_ [class_ "alert alert-success", role_ "alert"] [i|Zeit vom #{Time.formatTime german "%d.%m.%Y" date} gelöscht|]
                a_ [href_ [i|/activities/#{activityId}?active_tab=show_times|], class_ "btn btn-primary", role_ "button"] "Zurück"
