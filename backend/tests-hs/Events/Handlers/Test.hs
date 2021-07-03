module Events.Handlers.Test where

import Events.Handlers (getNewCheckboxes)
import Network.Wai.Parse
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "getNewCheckboxes"
    [ testCase "unchecked previous files" $ do
        let inputParams = [("allFiles", "foo:bar:bax")]
            inputFileParams = []

        actual <- getNewCheckboxes return Just (inputParams, inputFileParams)
        print actual
        actual @?= Right [("foo", "foo:bar:bax", False)],
      testCase "new files" $ do
        let inputParams = [("allFiles", "foo:bar:bax")]
            inputFileParams = [("eventAttachmentsInput", FileInfo "new" "pdf" "some/path")]

        actual <- getNewCheckboxes return Just (inputParams, inputFileParams)
        print actual
        actual @?= Right [("new", "new:pdf:some/path", True), ("foo", "foo:bar:bax", False)],
      testCase "previous checked" $ do
        let inputParams = [("allFiles", "foo:bar:bax"),("newFileCheckbox", "foo")]
            inputFileParams = [("eventAttachmentsInput", FileInfo "new" "pdf" "some/path")]

        actual <- getNewCheckboxes return Just (inputParams, inputFileParams)
        print actual
        actual @?= Right [("new", "new:pdf:some/path", True), ("foo", "foo:bar:bax", True)],
      testCase "previous checked, no files" $ do
        let inputParams = [("allFiles", "foo:bar:bax"),("allFiles", "some:other:file"),("newFileCheckbox", "foo")]
            inputFileParams = []

        actual <- getNewCheckboxes return Just (inputParams, inputFileParams)
        print actual
        actual @?= Right [("foo", "foo:bar:bax", True),("some", "some:other:file", False)]
    ]
