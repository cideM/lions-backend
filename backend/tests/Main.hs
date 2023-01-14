-- import qualified Events.Event.Event.Test
import qualified Feed.Message.Test
import qualified Login.Test
import qualified Password.Change.Test
import qualified Password.Reset.Test
import qualified Scrypt.Test
import qualified Session.Test
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Scrypt.Test.tests,
      testGroup
        "Password"
        [ Password.Reset.Test.tests,
          Password.Change.Test.tests
        ],
      Login.Test.tests,
      Session.Test.tests,
      Feed.Message.Test.tests
      -- testGroup
      --   "Events"
      --   [Events.Event.Event.Test.tests]
    ]
