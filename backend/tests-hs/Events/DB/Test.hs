module Events.DB.Test where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef
import qualified Data.Map.Strict as M
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Events.DB
import Events.Domain
import Helpers (as200, withDB, withFormRequest, withQueryString, withoutLogging)
import Network.AWS.SES.SendEmail
import Network.HTTP.Types.Status (status200)
import Network.URI.Encode (decode)
import Network.Wai.Test
import Test.Tasty
import Test.Tasty.HUnit
import Text.Email.Validate (emailAddress)
import User.Domain

tests :: TestTree
tests = testGroup "Events" [db]

db :: TestTree
db =
  testGroup
    "DB"
    [ testGroup
        "createAndAggregateEventsFromDb"
        [ testCase "correctly aggregates event replies" $ do
            now <- Time.getCurrentTime
            emailFoo <- case emailAddress "foo@bar.com" of
              Nothing -> assertFailure "couldn't parse email in test prep"
              Just v -> return v
            emailBar <- case emailAddress "bar@bar.com" of
              Nothing -> assertFailure "couldn't parse email in test prep"
              Just v -> return v
            let rows =
                  [ (GetEventRow 1 "Event One" now True "Some Event" "Location" (Just 1) (Just True) (Just 5) (Just "foo@bar.com")),
                    (GetEventRow 1 "Event One" now True "Some Event" "Location" (Just 1) (Just True) (Just 5) (Just "bar@bar.com")),
                    (GetEventRow 2 "Event One" now True "Some Event" "Location" (Just 1) (Just True) (Just 5) (Just "bar@bar.com"))
                  ]
                expected =
                  [ ( EventId 1,
                      Event
                        "Event One"
                        now
                        True
                        "Some Event"
                        "Location"
                        [ (Reply True (UserEmail emailBar) (UserId 1) 5),
                          (Reply True (UserEmail emailFoo) (UserId 1) 5)
                        ]
                    ),
                    ( EventId 2,
                      Event
                        "Event One"
                        now
                        True
                        "Some Event"
                        "Location"
                        [(Reply True (UserEmail emailBar) (UserId 1) 5)]
                    )
                  ]

            createAndAggregateEventsFromDb rows @?= (Right $ M.fromList expected)
        ]
    ]
