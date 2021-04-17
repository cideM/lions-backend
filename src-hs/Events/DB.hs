{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Events.DB (getAll, deleteReply, upsertReply) where

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
import Events.Domain (Event (..), EventId (..), Reply (..))
import User.DB (DBEmail (..))
import User.Domain (UserEmail (..), UserId (..))
import Prelude hiding (id)

data GetAllEventsRow = GetAllEventsRow
  { _eventId :: Int,
    _eventTitle :: Text,
    _eventDate :: Time.UTCTime,
    _eventFamilyAllowed :: Bool,
    _eventDescription :: Text,
    _eventLocation :: Text,
    _eventReplyUserId :: Int,
    _eventReplyComing :: Bool,
    _eventReplyNumGuests :: Int,
    _eventReplyEmail :: DBEmail
  }
  deriving (Show)

instance FromRow GetAllEventsRow where
  fromRow =
    GetAllEventsRow
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

getAll ::
  (MonadIO m, MonadThrow m) =>
  SQLite.Connection ->
  m (Map EventId Event)
getAll conn = do
  (rows :: [GetAllEventsRow]) <-
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
        join event_replies on events.id = eventid
        join users on userid = users.id
      |]
  return $
    foldr' f Map.empty rows
  where
    f GetAllEventsRow {_eventReplyEmail = (DBEmail email), ..} xs =
      let reply = Reply _eventReplyComing (UserEmail email) (UserId _eventReplyUserId) _eventReplyNumGuests
          alterFn Nothing =
            Just $
              Event
                _eventTitle
                _eventDate
                _eventFamilyAllowed
                _eventDescription
                _eventLocation
                [reply]
          alterFn (Just e@Event {..}) = Just $ e {eventReplies = reply : eventReplies}
       in Map.alter alterFn (EventId _eventId) xs

deleteReply ::
  (MonadIO m, MonadThrow m) =>
  SQLite.Connection ->
  EventId ->
  UserId ->
  m ()
deleteReply conn (EventId eventid) (UserId userid) =
  liftIO $ SQLite.execute conn "delete from event_replies where userid = ? and eventid = ?" [userid, eventid]

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
