module Events.Handlers.Test where

import qualified Events.Event as Events
import Events.Handlers (FileActions (..), getFileActions)
import Network.Wai.Parse
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "getFileActions"
    [ testCase "unchecked previous files" $ do
        let inputParams = [("allFiles", "foo:bar:bax")]
            inputFileParams = []

        (actions, encrypted) <- getFileActions return Just [] (inputParams, inputFileParams)

        actions
          @?= ( FileActions
                  { fileActionsKeep = [],
                    fileActionsDelete = [],
                    fileActionsDontUpload = [FileInfo "foo" "bar" "bax"],
                    fileActionsUpload = []
                  }
              )
        encrypted @?= ["foo:bar:bax"],
      testCase "new files" $ do
        let inputParams = [("allFiles", "foo:bar:bax")]
            inputFileParams = [("eventAttachmentsInput", FileInfo "new" "pdf" "some/path")]

        (actions, encrypted) <- getFileActions return Just [] (inputParams, inputFileParams)

        actions
          @?= ( FileActions
                  { fileActionsKeep = [],
                    fileActionsDelete = [],
                    fileActionsDontUpload = [FileInfo "foo" "bar" "bax"],
                    fileActionsUpload = [FileInfo "new" "pdf" "some/path"]
                  }
              )
        encrypted @?= ["foo:bar:bax", "new:pdf:some/path"],
      testCase "previous checked" $ do
        let inputParams = [("allFiles", "foo:bar:bax"), ("newFileCheckbox", "foo")]
            inputFileParams = [("eventAttachmentsInput", FileInfo "new" "pdf" "some/path")]

        (actions, encrypted) <- getFileActions return Just [] (inputParams, inputFileParams)

        actions
          @?= ( FileActions
                  { fileActionsKeep = [],
                    fileActionsDelete = [],
                    fileActionsDontUpload = [],
                    fileActionsUpload = [FileInfo "new" "pdf" "some/path", FileInfo "foo" "bar" "bax"]
                  }
              )
        encrypted @?= ["foo:bar:bax", "new:pdf:some/path"],
      testCase "previous checked, no files" $ do
        let inputParams = [("allFiles", "foo:bar:bax"), ("allFiles", "some:other:file"), ("newFileCheckbox", "foo")]
            inputFileParams = []

        (actions, encrypted) <- getFileActions return Just [] (inputParams, inputFileParams)

        actions
          @?= ( FileActions
                  { fileActionsKeep = [],
                    fileActionsDelete = [],
                    fileActionsDontUpload = [FileInfo "some" "other" "file"],
                    fileActionsUpload = [FileInfo "foo" "bar" "bax"]
                  }
              )
        encrypted @?= ["foo:bar:bax", "some:other:file"],
      testCase "with already uploaded not checked" $ do
        let inputParams = []
            inputFileParams = []
            alreadyUploaded = [Events.Attachment "foobar"]

        (actions, encrypted) <- getFileActions return Just alreadyUploaded (inputParams, inputFileParams)

        actions
          @?= ( FileActions
                  { fileActionsKeep = [],
                    fileActionsDelete = alreadyUploaded,
                    fileActionsDontUpload = [],
                    fileActionsUpload = []
                  }
              )
        encrypted @?= [],
      testCase "with already uploaded, checked" $ do
        let inputParams = [("newFileCheckbox", "foobar")]
            inputFileParams = []
            alreadyUploaded = [Events.Attachment "foobar"]

        (actions, encrypted) <- getFileActions return Just alreadyUploaded (inputParams, inputFileParams)

        actions
          @?= ( FileActions
                  { fileActionsKeep = alreadyUploaded,
                    fileActionsDelete = [],
                    fileActionsDontUpload = [],
                    fileActionsUpload = []
                  }
              )
        encrypted @?= []
    ]
