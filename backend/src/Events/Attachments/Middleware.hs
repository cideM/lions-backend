module Events.Attachments.Middleware (middleware) where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.ByteString (ByteString, readFile)
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import Network.HTTP.Types (status200)
import Network.Wai (pathInfo, responseLBS)
import qualified Network.Wai.Middleware.Static as Static
import System.FilePath ((</>))
import qualified UnliftIO
import qualified Wai
import Prelude hiding (id, readFile)

middleware ::
  ( UnliftIO.MonadUnliftIO m,
    MonadReader env m,
    App.HasEventStorage env,
    MonadThrow m,
    App.HasDb env
  ) =>
  Wai.MiddlewareT m
middleware next req send = do
  destinationDir <- asks App.getStorageDir
  conn <- asks App.getDb
  case pathInfo req of
    ["events", eventid, filenameArg] -> do
      rows <- liftIO $ SQLite.query conn [sql|select filename, content from event_attachments where eventid = ? and filename = ?|] (eventid, filenameArg)
      case rows of
        [row :: (Text, Maybe ByteString)] -> do
          let (filename, content) = row
          case content of
            Just actualContent -> do
              let mime = Static.mimeTypes Static.defaultOptions (Text.unpack filename)
              send $ responseLBS status200 [("Content-Type", mime)] (BL.fromChunks [actualContent])
            Nothing -> do
              let destDir = destinationDir </> (Text.unpack eventid)
              let dest = destDir </> (Text.unpack filename)
              actualContent <- liftIO $ readFile dest
              let mime = Static.mimeTypes Static.defaultOptions (Text.unpack filename)
              send $ responseLBS status200 [("Content-Type", mime)] (BL.fromChunks [actualContent])
        [] -> do
          next req send
        r -> do
          -- TODO: Does this actually throw? :(
          _ <- throwString [i|Unexpected rows for getting event attachment: #{r}|]
          next req send
    _ -> do
      next req send
