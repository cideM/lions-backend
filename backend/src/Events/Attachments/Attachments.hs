module Events.Attachments.Attachments
  ( save,
    removeAll,
    remove,
    makeActions,
    processActions,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.ByteString (ByteString)
import qualified Data.String.Interpolate as Interpolate
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Error as E
import qualified Events.Attachments.Saved as Saved
import qualified Events.Attachments.Temporary as Temporary
import qualified Events.Event as Events
import Events.Attachments.Actions (Actions(..))
import qualified Katip as K
import Network.Wai.Parse (FileInfo (..), Param)
import qualified System.Directory
import System.FilePath ((</>))
import qualified Web.ClientSession as ClientSession

save ::
  ( MonadIO m,
    App.HasEventStorage env,
    MonadReader env m
  ) =>
  Events.Id ->
  FilePath ->
  FilePath ->
  m ()
save (Events.Id eid) source destinationFileName = do
  destinationDir <- asks App.getStorageDir
  let destDir = destinationDir </> [Interpolate.i|#{eid}|]
  liftIO $ System.Directory.createDirectoryIfMissing True destDir

  let dest = destDir </> destinationFileName

  liftIO $ System.Directory.copyFile source dest

removeAll ::
  ( MonadIO m,
    App.HasEventStorage env,
    MonadReader env m
  ) =>
  Events.Id ->
  m ()
removeAll (Events.Id eid) = do
  destinationDir <- asks App.getStorageDir
  liftIO $ System.Directory.removeDirectoryRecursive $ destinationDir </> show eid

remove ::
  ( MonadIO m,
    App.HasEventStorage env,
    MonadReader env m
  ) =>
  Events.Id ->
  FilePath ->
  m ()
remove (Events.Id eid) filename = do
  destinationDir <- asks App.getStorageDir
  liftIO $ System.Directory.removeFile $ destinationDir </> show eid </> filename

-- This is the dual to makeActions, which makes the necessary changes
processActions ::
  ( MonadIO m,
    K.KatipContext m,
    App.HasEventStorage env,
    MonadReader env m,
    MonadThrow m
  ) =>
  Events.Id ->
  Actions ->
  m ()
processActions eid actions@Actions {..} = do
  K.katipAddContext (K.sl "file_actions" actions) $ do
    forM_ actionsUpload $ \(Temporary.Attachment {..}) -> do
      K.logLocM K.DebugS "saving file"
      save eid attachmentFilePath $ Text.unpack attachmentFileName

      K.logLocM K.DebugS "removing temp file"
      liftIO $ System.Directory.removeFile attachmentFilePath

    forM_ actionsDelete $ \Saved.Attachment {..} -> do
      K.logLocM K.DebugS "removing attachment"
      remove eid $ Text.unpack attachmentFileName

    forM_ actionsDontUpload $ \(Temporary.Attachment {..}) -> do
      K.logLocM K.DebugS "removing temp file"
      liftIO $ System.Directory.removeFile attachmentFilePath

makeActions ::
  ( MonadIO m,
    K.KatipContext m,
    App.HasSessionEncryptionKey env,
    MonadReader env m,
    MonadThrow m
  ) =>
  [Saved.Attachment] ->
  ([Param], [(ByteString, FileInfo FilePath)]) ->
  m (Actions, [ByteString])
makeActions alreadySaved body = do
  pastFileInfos <-
    E.runExceptT parseEncryptedFileInfo >>= \case
      Left e -> throwString $ Text.unpack e
      Right v -> return v

  K.katipAddContext (K.sl "file_infos" pastFileInfos) $ do
    -- Turn the file upload params from the request body into AttachmentInfo,
    -- after filtering out those weird empty string files
    let newFileInfos =
          map
            ( \FileInfo {..} ->
                Temporary.Attachment
                  { attachmentFileName = (decodeUtf8 fileName),
                    attachmentFileContentType = (decodeUtf8 fileContentType),
                    attachmentFilePath = fileContent
                  }
            )
            . filter ((/=) "\"\"" . fileName)
            . map snd
            $ snd body

        checked = map (decodeUtf8 . snd) . filter ((==) "newFileCheckbox" . fst) $ fst body
        isChecked = flip elem checked
        keep = filter (isChecked . Saved.attachmentFileName) alreadySaved
        savedButDelete = filter (not . isChecked . Saved.attachmentFileName) alreadySaved
        notSavedAndDelete = filter (not . isChecked . Temporary.attachmentFileName) pastFileInfos

        upload =
          newFileInfos
            ++ ( filter
                   (isChecked . Temporary.attachmentFileName)
                   pastFileInfos
               )

    K.logLocM K.DebugS "inside parse request"

    encryptedFileInfos <- makeEncryptedFileInfo $ pastFileInfos ++ newFileInfos

    let actions = Actions keep savedButDelete notSavedAndDelete upload

    return (actions, encryptedFileInfos)
  where
    -- The reverse of parseDecryptedFileInfo
    makeEncryptedFileInfo unencrypted = do
      sessionKey <- asks App.getSessionEncryptionKey
      traverse
        ( \( Temporary.Attachment
               { attachmentFileName = fileName,
                 attachmentFileContentType = fileContentType,
                 attachmentFilePath = filePath
               }
             ) ->
              let s = [Interpolate.i|#{fileName}:#{fileContentType}:#{filePath}|]
               in liftIO $ ClientSession.encryptIO sessionKey s
        )
        unencrypted

    -- Input was already decrypted with session key, we just need to split on
    -- colon more or less
    parseDecryptedFileInfo s =
      let [filename, filetype, filepath] = Text.split (':' ==) $ decodeUtf8 s
       in (filename, filetype, filepath)

    -- Decrypt the colon delimited strings with the session key, from the
    -- request body
    parseEncryptedFileInfo ::
      ( MonadIO m,
        E.MonadError Text m,
        App.HasSessionEncryptionKey env,
        MonadReader env m
      ) =>
      m [(Temporary.Attachment)]
    parseEncryptedFileInfo = do
      sessionKey <- asks App.getSessionEncryptionKey

      -- Get the form parameters from body
      let encryptedHiddenInputs = map snd . filter ((==) "allFiles" . fst) $ fst body

      -- Decrypt the hidden input field values, parse the special strings into
      -- proper data types
      case traverse (ClientSession.decrypt sessionKey) encryptedHiddenInputs of
        Nothing -> E.throwError "couldn't decrypt hidden inputs"
        Just decrypted ->
          return
            . map
              ( \(name, filetype, path) ->
                  Temporary.Attachment name filetype (Text.unpack path)
              )
            $ map parseDecryptedFileInfo decrypted
