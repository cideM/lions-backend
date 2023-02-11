module Feed.Middleware where

import qualified App
import Control.Exception.Safe
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Feed.DB
import Feed.Message
import Network.HTTP.Types (status200)
import Network.Wai (pathInfo, responseLBS)
import qualified Network.Wai.Middleware.Static as Static
import Text.Read (readEither)
import qualified UnliftIO
import qualified Wai
import Prelude hiding (id)

middleware ::
  ( UnliftIO.MonadUnliftIO m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Wai.MiddlewareT m
middleware next req send = do
  conn <- asks App.getDb

  case pathInfo req of
    ["news", postIdArg, filenameArg] -> do
      let tryParse = readEither $ Text.unpack postIdArg

      case tryParse of
        Left e -> do
          let err = "couldn't parse '" <> Text.unpack postIdArg <> "' as number: " <> e
          throwString err
        Right postId -> do
          maybeAttachment <- fetchAttachmentWithContent conn (Id postId) filenameArg

          case maybeAttachment of
            Nothing -> do
              let err = "no attachment for feed post: " <> show postId
              throwString err
            Just (filename, content) -> do
              let mime = Static.mimeTypes Static.defaultOptions (Text.unpack filename)

              send $
                responseLBS
                  status200
                  [("Content-Type", mime)]
                  (BL.fromChunks [content])
    _ -> do
      next req send
