module Events.DB.Test where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef
import qualified Data.Map.Strict as M
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import Events.DB
import qualified Events.Types as Events
import Helpers (as200, withDB, withFormRequest, withQueryString, withoutLogging)
import Network.AWS.SES.SendEmail
import Network.HTTP.Types.Status (status200)
import Network.URI.Encode (decode)
import Network.Wai.Test
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Email.Validate as Email
import User.Types

tests :: TestTree
tests = testGroup "Events" [db]

db :: TestTree
db =
  testGroup
    "DB"
    [ testCase "getAll" $ do
        withDB $ \conn -> do
          let EventDb {..} = newEventDb conn
          SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"
          SQLite.execute_
            conn
            [sql|
            insert into events (title, date, family_allowed, description, location)
            values ('some event', '2021-07-10 14:00:00', 1, 'some desc', 'some loc')|]
          SQLite.execute_
            conn
            [sql|
            insert into event_replies (userid, eventid, coming, guests)
            values (1, 1, 1, 2)|]
          SQLite.execute_
            conn
            [sql|
            insert into event_attachments (eventid, filename)
            values (1, 'some file')|]

          actual <- eventDbAll

          (date :: Time.UTCTime) <- case Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %T" "2021-07-10 14:00:00" of
            Nothing -> fail "couldn't parse time"
            Just v -> return v

          let email = Email.unsafeEmailAddress "foo" "bar.com"

          actual
            @?= [ ( Events.Id 1,
                    Events.Event
                      "some event"
                      date
                      True
                      "some desc"
                      "some loc"
                      [Events.Reply True (UserEmail email) (UserId 1) 2]
                      [Events.Attachment "some file"]
                  )
                ]
    ]
