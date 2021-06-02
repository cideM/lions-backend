{-# LANGUAGE OverloadedStrings #-}

import qualified LoginTests
import qualified PasswordResetTests
import qualified ScryptTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ ScryptTest.tests,
      PasswordResetTests.tests,
      LoginTests.tests
    ]
