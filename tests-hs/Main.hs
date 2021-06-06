import qualified Events.DB.Test
import qualified Session.Session.Test
import qualified Login.Test
import qualified PasswordReset.Test
import qualified Scrypt.Test
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Scrypt.Test.tests,
      PasswordReset.Test.tests,
      Login.Test.tests,
      Session.Session.Test.tests,
      Events.DB.Test.tests
    ]
