import qualified Events.DBTests
import qualified Login.Test
import qualified PasswordReset.Test
import qualified ScryptTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ ScryptTest.tests,
      PasswordReset.Test.tests,
      Login.Test.tests,
      Events.DBTests.tests
    ]
