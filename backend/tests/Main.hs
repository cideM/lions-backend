import qualified Scrypt.Test
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [Scrypt.Test.tests]
