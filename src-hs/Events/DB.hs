{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Events.DB
  ( getEvent,
    getAll,
    deleteReply,
    createEvent,
    updateEvent,
    upsertReply,
    deleteEvent,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldr')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.QQ (sql)
import Debug.Trace
import Events.Domain (Event (..), EventCreate (..), EventId (..), Reply (..))
import User.DB (DBEmail (..))
import User.Domain (UserEmail (..), UserId (..))
import Prelude hiding (id)

data GetEventRow = GetEventRow
  { _eventId :: Int,
    _eventTitle :: Text,
    _eventDate :: Time.UTCTime,
    _eventFamilyAllowed :: Bool,
    _eventDescription :: Text,
    _eventLocation :: Text,
    _eventReplyUserId :: Maybe Int,
    _eventReplyComing :: Maybe Bool,
    _eventReplyNumGuests :: Maybe Int,
    _eventReplyEmail :: Maybe DBEmail
  }
  deriving (Show)

instance FromRow GetEventRow where
  fromRow =
    GetEventRow
      <$> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field
        <*> SQLite.field

createEvent ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  SQLite.Connection ->
  EventCreate ->
  m ()
createEvent conn EventCreate {..} = do
  liftIO $
    SQLite.execute
      conn
      [sql|
        insert into events (
          title,
          date,
          family_allowed,
          description,
          location
        ) values (?,?,?,?,?)
      |]
      (eventCreateTitle, eventCreateDate, eventCreateFamilyAllowed, eventCreateDescription, eventCreateLocation)

makeEvents :: GetEventRow -> Map EventId Event -> Map EventId Event
makeEvents GetEventRow {..} xs =
  let eventWithoutReplies =
        Event
          _eventTitle
          _eventDate
          _eventFamilyAllowed
          _eventDescription
          _eventLocation
      replyRaw =
        (,,,)
          <$> _eventReplyUserId
          <*> _eventReplyComing
          <*> _eventReplyEmail
          <*> _eventReplyNumGuests
      alterFn Nothing Nothing = Just $ eventWithoutReplies []
      alterFn (Just reply) Nothing = Just $ eventWithoutReplies [reply]
      alterFn Nothing (Just e) = Just e
      alterFn (Just reply) (Just e@Event {..}) = Just e {eventReplies = reply : eventReplies}
   in case replyRaw of
        -- If one of the reply fields is Nothing assume they're all Nothing.
        -- Not sure how to best handle this but if I do a left join and only
        -- get the left part of the select then all other fields will be null
        Nothing -> Map.alter (alterFn Nothing) (EventId _eventId) xs
        Just (replyUserId, replyComing, DBEmail replyEmail, replyNumGuests) ->
          let reply = Reply replyComing (UserEmail replyEmail) (UserId replyUserId) replyNumGuests
           in Map.alter (alterFn $ Just reply) (EventId _eventId) xs

getEvent ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  SQLite.Connection ->
  EventId ->
  m (Maybe Event)
getEvent conn (EventId eventid) = do
  rows <-
    liftIO $
      SQLite.query
        conn
        [sql|
        select events.id as eventid,
               title,
               date,
               family_allowed,
               description,
               location,
               userid,
               coming,
               guests,
               users.email as email
        from events
        left join event_replies on events.id = eventid
        left join users on userid = users.id
        where events.id = ?
      |]
        [eventid]
  let events = foldr' makeEvents Map.empty rows
  case Map.toList (traceShowId events) of
    [] -> return Nothing
    [x] -> return . Just $ snd x
    v -> throwString $ "got more than one result from getEvent" <> show v

getAll ::
  (MonadIO m, MonadThrow m) =>
  SQLite.Connection ->
  m (Map EventId Event)
getAll conn = do
  (rows :: [GetEventRow]) <-
    liftIO $
      SQLite.query_
        conn
        [sql|
        select events.id as eventid,
               title,
               date,
               family_allowed,
               description,
               location,
               userid,
               coming,
               guests,
               users.email as email
        from events
        left join event_replies on events.id = eventid
        left join users on userid = users.id
      |]
  return $
    foldr' makeEvents Map.empty rows

deleteReply ::
  (MonadIO m, MonadThrow m) =>
  SQLite.Connection ->
  EventId ->
  UserId ->
  m ()
deleteReply conn (EventId eventid) (UserId userid) =
  liftIO $ SQLite.execute conn "delete from event_replies where userid = ? and eventid = ?" [userid, eventid]

deleteEvent ::
  (MonadIO m, MonadThrow m) =>
  SQLite.Connection ->
  EventId ->
  m ()
deleteEvent conn (EventId eventid) = do
  liftIO $ SQLite.execute conn "delete from event_replies where eventid = ?" [eventid]
  liftIO $ SQLite.execute conn "delete from events where id = ?" [eventid]

updateEvent ::
  (MonadIO m, MonadThrow m) =>
  SQLite.Connection ->
  EventId ->
  EventCreate ->
  m ()
updateEvent conn (EventId eventid) EventCreate {..} =
  liftIO $
    SQLite.execute
      conn
      [sql|
        update events
        set
          title = ?,
          date = ?,
          family_allowed = ?,
          description = ?,
          location = ?
        where id = ?
      |]
      (eventCreateTitle, eventCreateDate, eventCreateFamilyAllowed, eventCreateDescription, eventCreateLocation, eventid)

upsertReply ::
  (MonadIO m, MonadThrow m) =>
  SQLite.Connection ->
  EventId ->
  Reply ->
  m ()
upsertReply conn (EventId eventid) (Reply coming _ (UserId userid) guests) =
  liftIO $
    SQLite.execute
      conn
      [sql|
    insert into event_replies (userid, eventid, coming, guests)
    values (?,?,?,?)
    on conflict (userid,eventid) do update set
      coming=excluded.coming,
      guests=excluded.guests
    where userid = ? and eventid = ?
  |]
      [userid, eventid, if coming then 1 else 0, guests, userid, eventid]
