module Events.Attachments.Actions
  ( Actions (..),
    apply,
    make,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.String.Interpolate as Interpolate
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Error as E
import qualified Events.Attachments.Saved as Saved
import qualified Events.Attachments.Temporary as Temporary
import qualified Events.Event.Id as Event
import GHC.Generics
import qualified Katip as K
import Network.Wai.Parse (FileInfo (..), Param)
import qualified System.Directory
import qualified Web.ClientSession as ClientSession

lower1 :: String -> String
lower1 (c : cs) = Char.toLower c : cs
lower1 [] = []

data Actions = Actions
  { actionsKeep :: [Saved.FileName],
    actionsDelete :: [Saved.FileName],
    actionsDontUpload :: [Temporary.Attachment],
    actionsUpload :: [Temporary.Attachment]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Actions where
  parseJSON = Aeson.genericParseJSON defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 7}

instance ToJSON Actions where
  toEncoding = genericToEncoding defaultOptions {Aeson.fieldLabelModifier = lower1 . drop 7}

-- This is the dual to makeActions, which makes the necessary changes
apply ::
  ( MonadIO m,
    K.KatipContext m,
    App.HasEventStorage env,
    MonadReader env m,
    MonadThrow m
  ) =>
  Event.Id ->
  Actions ->
  m ()
apply _ actions@Actions {..} = do
  K.katipAddContext (K.sl "file_actions" actions) $ do
    -- Why is this even needed?
    forM_ actionsDontUpload $ \(Temporary.Attachment {..}) -> do
      K.logLocM K.DebugS "removing temp file"
      liftIO $ System.Directory.removeFile attachmentFilePath

make ::
  ( MonadIO m,
    K.KatipContext m,
    App.HasSessionEncryptionKey env,
    MonadReader env m,
    MonadThrow m
  ) =>
  [Saved.FileName] ->
  ([Param], [(ByteString, FileInfo FilePath)]) ->
  m
    ( Actions,
      -- Encrypted file infos as a string
      [ByteString]
    )
make alreadySaved body = do
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
        keep = filter (isChecked . Saved.unFileName) alreadySaved
        savedButDelete = filter (not . isChecked . Saved.unFileName) alreadySaved
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
