module Events.Attachments
  ( save,
    removeAll,
    remove,
    parseRequest,
    FileActions (..),
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.ByteString (ByteString)
import qualified Data.String.Interpolate as Interpolate
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Events.Types as Events
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

parseDecryptedString :: ByteString -> (Text, Text, Text)
parseDecryptedString s =
  let [filename, filetype, filepath] = Text.split (':' ==) $ decodeUtf8 s
   in (filename, filetype, filepath)

getEncryptedFileInfo ::
  (ByteString -> Maybe ByteString) ->
  ([Param], [(ByteString, FileInfo FilePath)]) ->
  Either Text.Text [(FileInfo FilePath)]
getEncryptedFileInfo decrypt body =
  let encryptedHiddenInputs = map snd . filter ((==) "allFiles" . fst) $ fst body
   in case traverse decrypt encryptedHiddenInputs of
        Nothing -> Left "couldn't decrypt hidden inputs"
        Just decrypted ->
          let parsed = map parseDecryptedString decrypted
              fileinfos = map (\(name, filetype, path) -> FileInfo (encodeUtf8 name) (encodeUtf8 filetype) (Text.unpack path)) parsed
           in Right fileinfos

getNewFileInfo :: ([Param], [(ByteString, FileInfo FilePath)]) -> [(FileInfo FilePath)]
getNewFileInfo = filter ((/=) "\"\"" . fileName) . map snd . snd

encryptFileInfo ::
  (ByteString -> IO ByteString) ->
  [(FileInfo FilePath)] ->
  IO [ByteString]
encryptFileInfo encrypt =
  traverse (\FileInfo {..} -> encrypt [Interpolate.i|#{fileName}:#{fileContentType}:#{fileContent}|])

getCheckedFromBody :: ([Param], [(ByteString, FileInfo FilePath)]) -> [Text]
getCheckedFromBody = map (decodeUtf8 . snd) . filter ((==) "newFileCheckbox" . fst) . fst

data FileActions = FileActions
  { fileActionsKeep :: [Events.Attachment],
    fileActionsDelete :: [Events.Attachment],
    fileActionsDontUpload :: [FileInfo FilePath],
    fileActionsUpload :: [FileInfo FilePath]
  }
  deriving (Show, Eq)

parseRequest ::
  ( MonadIO m,
    App.HasSessionEncryptionKey env,
    MonadReader env m,
    MonadThrow m
  ) =>
  [Events.Attachment] ->
  ([Param], [(ByteString, FileInfo FilePath)]) ->
  m (FileActions, [ByteString])
parseRequest alreadySaved body = do
  sessionKey <- asks App.getSessionEncryptionKey
  let clientEncrypt = ClientSession.encryptIO sessionKey
      clientDecrypt = ClientSession.decrypt sessionKey

  pastFileInfos <- case getEncryptedFileInfo clientDecrypt body of
    Left e -> throwString $ Text.unpack e
    Right v -> return v

  let newFileInfos = getNewFileInfo body
      checked = getCheckedFromBody body
      keep = filter (flip elem checked . Events.attachmentFileName) alreadySaved
      savedButDelete = filter (flip notElem checked . Events.attachmentFileName) alreadySaved
      notSavedAndDelete = filter (flip notElem checked . decodeUtf8 . fileName) pastFileInfos
      upload = newFileInfos ++ (filter (flip elem checked . decodeUtf8 . fileName) pastFileInfos)

  encryptedFileInfos <- liftIO $ encryptFileInfo clientEncrypt $ pastFileInfos ++ newFileInfos

  let actions = FileActions keep savedButDelete notSavedAndDelete upload

  return (actions, encryptedFileInfos)
