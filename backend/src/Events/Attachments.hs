module Events.Attachments
  ( save,
    removeAll,
    remove,
    middleware,
    makeFileActions,
    processFileActions,
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
import Events.FileActions (FileActions (..))
import qualified Events.AttachmentInfo as Events
import qualified Events.Types as Events
import qualified Katip as K
import Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Static
import Network.Wai.Parse (FileInfo (..), Param)
import qualified System.Directory
import System.FilePath ((</>))
import qualified UnliftIO
import qualified Wai.Class as Wai
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

-- This is the dual to makeFileActions, which makes the necessary changes
processFileActions ::
  ( MonadIO m,
    K.KatipContext m,
    App.HasEventStorage env,
    MonadReader env m,
    MonadThrow m
  ) =>
  Events.Id ->
  FileActions ->
  m ()
processFileActions eid actions@FileActions {..} = do
  K.katipAddContext (K.sl "file_actions" actions) $ do
    forM_ fileActionsUpload $ \(Events.AttachmentInfo {..}) -> do
      K.logLocM K.DebugS "saving file"
      save eid attachmentInfoFilePath $ Text.unpack attachmentInfoFileName

      K.logLocM K.DebugS "removing temp file"
      liftIO $ System.Directory.removeFile attachmentInfoFilePath

    forM_ fileActionsDelete $ \Events.Attachment {..} -> do
      K.logLocM K.DebugS "removing attachment"
      remove eid $ Text.unpack attachmentFileName

    forM_ fileActionsDontUpload $ \(Events.AttachmentInfo {..}) -> do
      K.logLocM K.DebugS "removing temp file"
      liftIO $ System.Directory.removeFile attachmentInfoFilePath

makeFileActions ::
  ( MonadIO m,
    K.KatipContext m,
    App.HasSessionEncryptionKey env,
    MonadReader env m,
    MonadThrow m
  ) =>
  [Events.Attachment] ->
  ([Param], [(ByteString, FileInfo FilePath)]) ->
  m (FileActions, [ByteString])
makeFileActions alreadySaved body = do
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
                Events.AttachmentInfo
                  { attachmentInfoFileName = (decodeUtf8 fileName),
                    attachmentInfoFileContentType = (decodeUtf8 fileContentType),
                    attachmentInfoFilePath = fileContent
                  }
            )
            . filter ((/=) "\"\"" . fileName)
            . map snd
            $ snd body

        checked = map (decodeUtf8 . snd) . filter ((==) "newFileCheckbox" . fst) $ fst body
        isChecked = flip elem checked
        keep = filter (isChecked . Events.attachmentFileName) alreadySaved
        savedButDelete = filter (not . isChecked . Events.attachmentFileName) alreadySaved
        notSavedAndDelete = filter (not . isChecked . Events.attachmentInfoFileName) pastFileInfos

        upload =
          newFileInfos
            ++ ( filter
                   (isChecked . Events.attachmentInfoFileName)
                   pastFileInfos
               )

    K.logLocM K.DebugS "inside parse request"

    encryptedFileInfos <- makeEncryptedFileInfo $ pastFileInfos ++ newFileInfos

    let actions = FileActions keep savedButDelete notSavedAndDelete upload

    return (actions, encryptedFileInfos)
  where
    -- The reverse of parseDecryptedFileInfo
    makeEncryptedFileInfo unencrypted = do
      sessionKey <- asks App.getSessionEncryptionKey
      traverse
        ( \( Events.AttachmentInfo
               { attachmentInfoFileName = fileName,
                 attachmentInfoFileContentType = fileContentType,
                 attachmentInfoFilePath = filePath
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
      m [(Events.AttachmentInfo)]
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
                  Events.AttachmentInfo name filetype (Text.unpack path)
              )
            $ map parseDecryptedFileInfo decrypted

middleware :: (UnliftIO.MonadUnliftIO m) => String -> Wai.MiddlewareT m
middleware storageDir = do
  Wai.liftMiddleware
    ( Static.staticPolicy
        ( Static.policy rewriteEvents >-> Static.addBase storageDir
        )
    )
  where
    -- Turn /events/2/foo.pdf -> /2/foo.pdf
    -- since that's the actual path inside the event attachment storage folder
    rewriteEvents :: String -> Maybe String
    rewriteEvents s =
      let t = Text.pack s
       in if "events/" `Text.isPrefixOf` t
            then Just (Text.unpack $ Text.replace "events/" "" t)
            else Nothing
