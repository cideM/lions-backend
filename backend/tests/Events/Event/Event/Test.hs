module Events.Event.Event.Test where

import qualified App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import qualified Events.Attachments.Saved as Saved
import qualified Events.Event.Event as Event
import qualified Events.Event.Id as Event
import qualified Events.Reply.Reply as Event
import Helpers
  ( withTestEnvProd,
  )
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Email.Validate as Email
import User.Types

tests :: TestTree
tests =
  testGroup
    "Event"
    [ testCase "getAll" $ do
        withTestEnvProd $ \_ -> do
          conn <- asks App.getDb

          liftIO $ SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"

          liftIO $
            SQLite.execute_
              conn
              [sql|
            insert into events (title, date, family_allowed, description, location)
            values ('some event', '2021-07-10 14:00:00', 1, 'some desc', 'some loc')|]

          liftIO $
            SQLite.execute_
              conn
              [sql|
            insert into event_replies (userid, eventid, coming, guests)
            values (1, 1, 1, 2)|]

          liftIO $
            SQLite.execute_
              conn
              [sql|
            insert into event_attachments (eventid, filename)
            values (1, 'some file')|]

          actual <- Event.getAll

          (date :: Time.UTCTime) <- liftIO $ case Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %T" "2021-07-10 14:00:00" of
            Nothing -> fail "couldn't parse time"
            Just v -> return v

          let email = Email.unsafeEmailAddress "foo" "bar.com"

          liftIO $
            actual
              @?= [ ( Event.Id 1,
                      Event.Event
                        "some event"
                        date
                        True
                        "some desc"
                        "some loc"
                        [Event.Reply True (UserEmail email) (UserId 1) 2]
                        [Saved.FileName "some file"]
                    )
                  ],
      testCase "get" $ do
        withTestEnvProd $ \_ -> do
          conn <- asks App.getDb

          liftIO $ SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"

          liftIO $
            SQLite.execute_
              conn
              [sql|insert into events (title, date, family_allowed, description, location)
                   values ('some event', '2021-07-10 14:00:00', 1, 'some desc', 'some loc')|]

          liftIO $
            SQLite.execute_
              conn
              [sql|insert into event_replies (userid, eventid, coming, guests)
                   values (1, 1, 1, 2)|]

          liftIO $
            SQLite.execute_
              conn
              [sql| insert into event_attachments (eventid, filename)
                    values (1, 'some file')|]

          actual <- Event.get (Event.Id 1)

          (date :: Time.UTCTime) <- liftIO $ case Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %T" "2021-07-10 14:00:00" of
            Nothing -> fail "couldn't parse time"
            Just v -> return v

          let email = Email.unsafeEmailAddress "foo" "bar.com"

          liftIO $
            actual
              @?= ( Just $
                      Event.Event
                        "some event"
                        date
                        True
                        "some desc"
                        "some loc"
                        [Event.Reply True (UserEmail email) (UserId 1) 2]
                        [Saved.FileName "some file"]
                  )
    ]
