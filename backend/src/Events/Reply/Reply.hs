module Events.Reply.Reply
  ( Reply (..),
    upsert,
    delete,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
import qualified Data.Char as Char
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import Events.Event.Id (Id (..))
import GHC.Generics
import qualified User.Id as User
import qualified User.Email as UserEmail
import Prelude hiding (id)

-- Now that I'm writing tests that involve this data type I find it a bit weird
-- that it doesn't have the event ID. Strictly speaking a reply is meaningless
-- without its event ID.
data Reply = Reply
  { replyComing :: Bool,
    replyUserEmail :: UserEmail.Email,
    replyUserId :: User.Id,
    replyGuests :: Int
  }
  deriving (Show, Generic, Eq)

instance FromJSON Reply where
  parseJSON = A.genericParseJSON defaultOptions {A.fieldLabelModifier = lower1 . drop 5}

instance ToJSON Reply where
  toEncoding = genericToEncoding defaultOptions {A.fieldLabelModifier = lower1 . drop 5}

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []

upsert ::
  ( MonadIO m,
    MonadReader env m,
    App.HasDb env
  ) =>
  Id ->
  Reply ->
  m ()
upsert (Id eventid) (Reply coming _ (User.Id userid) guests) = do
  conn <- asks App.getDb
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

delete ::
  ( MonadIO m,
    Monad m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Id ->
  User.Id ->
  m ()
delete (Id eventid) (User.Id userid) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "delete from event_replies where userid = ? and eventid = ?" [userid, eventid]
