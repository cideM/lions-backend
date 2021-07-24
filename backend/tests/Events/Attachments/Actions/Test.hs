module Events.Attachments.Actions.Test where

import qualified App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import qualified Events.Attachments.Actions as Actions
import qualified Events.Attachments.Saved as Saved
import qualified Events.Attachments.Temporary as Temporary
import Helpers
  ( withTestEnvProd,
  )
import qualified Katip as K
import Network.Wai.Parse
import Test.Tasty
import Test.Tasty.HUnit
import qualified Web.ClientSession as ClientSession

tests :: TestTree
tests = testGroup "Attachments" [actionsTests]

actionsTests :: TestTree
actionsTests = testGroup "Actions" [makeTests]

makeTests :: TestTree
makeTests =
  testGroup
    "make"
    [ testCase "allFiles" $ do
        withTestEnvProd $ \_ -> do
          K.katipNoLogging $ do
            sessionKey <- asks App.getSessionEncryptionKey

            encryptedInput <- liftIO $ ClientSession.encryptIO sessionKey "foo:bar:bax"

            let inputParams = [("allFiles", encryptedInput)]
                inputFileParams = []

            (actions, encrypted) <- Actions.make [] (inputParams, inputFileParams)

            let expectedActions =
                  ( Actions.Actions
                      { actionsKeep = [],
                        actionsDelete = [],
                        actionsDontUpload = [Temporary.Attachment "foo" "bar" "bax"],
                        actionsUpload = []
                      }
                  )

            liftIO $ actions @?= expectedActions

            liftIO $ map (ClientSession.decrypt sessionKey) encrypted @?= [Just "foo:bar:bax"],
      testCase "allFiles and eventAttachmentsInput" $ do
        withTestEnvProd $ \_ -> do
          K.katipNoLogging $ do
            sessionKey <- asks App.getSessionEncryptionKey

            encryptedInput <- liftIO $ ClientSession.encryptIO sessionKey "foo:bar:bax"

            let inputParams = [("allFiles", encryptedInput)]
                inputFileParams = [("eventAttachmentsInput", FileInfo "new" "pdf" "some/path")]

            (actions, encrypted) <- Actions.make [] (inputParams, inputFileParams)

            let expectedActions =
                  ( Actions.Actions
                      { actionsKeep = [],
                        actionsDelete = [],
                        actionsDontUpload = [Temporary.Attachment "foo" "bar" "bax"],
                        actionsUpload = [Temporary.Attachment "new" "pdf" "some/path"]
                      }
                  )

            liftIO $ actions @?= expectedActions

            liftIO $ map (ClientSession.decrypt sessionKey) encrypted @?= [Just "foo:bar:bax", Just "new:pdf:some/path"],
      testCase "allFiles, newFileCheckbox and eventAttachmentsInput" $ do
        withTestEnvProd $ \_ -> do
          K.katipNoLogging $ do
            sessionKey <- asks App.getSessionEncryptionKey

            encryptedInput <- liftIO $ ClientSession.encryptIO sessionKey "foo:bar:bax"

            let inputParams = [("allFiles", encryptedInput), ("newFileCheckbox", "foo")]
                inputFileParams = [("eventAttachmentsInput", FileInfo "new" "pdf" "some/path")]

            (actions, encrypted) <- Actions.make [] (inputParams, inputFileParams)

            liftIO $
              actions
                @?= ( Actions.Actions
                        { actionsKeep = [],
                          actionsDelete = [],
                          actionsDontUpload = [],
                          actionsUpload = [Temporary.Attachment "new" "pdf" "some/path", Temporary.Attachment "foo" "bar" "bax"]
                        }
                    )

            liftIO $ map (ClientSession.decrypt sessionKey) encrypted @?= [Just "foo:bar:bax", Just "new:pdf:some/path"],
      testCase "allFiles, newFileCheckbox" $ do
        withTestEnvProd $ \_ -> do
          K.katipNoLogging $ do
            sessionKey <- asks App.getSessionEncryptionKey

            encryptedInput <- liftIO $ ClientSession.encryptIO sessionKey "foo:bar:bax"
            encryptedInput2 <- liftIO $ ClientSession.encryptIO sessionKey "some:other:file"

            let inputParams = [("allFiles", encryptedInput), ("allFiles", encryptedInput2), ("newFileCheckbox", "foo")]
                inputFileParams = []

            (actions, encrypted) <- Actions.make [] (inputParams, inputFileParams)

            liftIO $actions
              @?= ( Actions.Actions
                      { actionsKeep = [],
                        actionsDelete = [],
                        actionsDontUpload = [Temporary.Attachment "some" "other" "file"],
                        actionsUpload = [Temporary.Attachment "foo" "bar" "bax"]
                      }
                  )

            liftIO $ map (ClientSession.decrypt sessionKey) encrypted @?= [Just "foo:bar:bax", Just "some:other:file"],
      testCase "with existing attachments but unchecked" $ do
        withTestEnvProd $ \_ -> do
          K.katipNoLogging $ do
            let inputParams = []
                inputFileParams = []
                alreadyUploaded = [Saved.FileName "foobar"]

            (actions, encrypted) <- Actions.make alreadyUploaded (inputParams, inputFileParams)

            liftIO $
              actions
                @?= ( Actions.Actions
                        { actionsKeep = [],
                          actionsDelete = alreadyUploaded,
                          actionsDontUpload = [],
                          actionsUpload = []
                        }
                    )

            liftIO $ encrypted @?= [],
      testCase "with existing attachments, checked" $ do
        withTestEnvProd $ \_ -> do
          K.katipNoLogging $ do
            let inputParams = [("newFileCheckbox", "foobar")]
                inputFileParams = []
                alreadyUploaded = [Saved.FileName "foobar"]

            (actions, encrypted) <- Actions.make alreadyUploaded (inputParams, inputFileParams)

            liftIO $
              actions
                @?= ( Actions.Actions
                        { actionsKeep = alreadyUploaded,
                          actionsDelete = [],
                          actionsDontUpload = [],
                          actionsUpload = []
                        }
                    )

            liftIO $ encrypted @?= []
    ]
