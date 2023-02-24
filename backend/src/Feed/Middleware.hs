module Feed.Middleware where

import qualified App
import Control.Exception.Safe
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Feed.DB
import Feed.Message
import Layout (LayoutStub (..), layout, warning)
import Lucid
import Network.HTTP.Types (status200, status404)
import Network.Wai (pathInfo, responseLBS)
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Static as Static
import Text.Read (readEither)
import qualified UnliftIO
import qualified User.Session
import qualified Wai
import Prelude hiding (id)

middleware ::
  ( UnliftIO.MonadUnliftIO m,
    MonadReader env m,
    MonadThrow m,
    App.HasSessionDataVaultKey env,
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
              sessionDataVaultKey <- asks App.getSessionDataVaultKey
              let vault = Wai.vault req
                  authInfo = User.Session.fromVault sessionDataVaultKey vault

              send
                . responseLBS status404 [("Content-Type", "text/html; charset=UTF-8")]
                . renderBS
                . layout authInfo Nothing
                . LayoutStub "Nicht gefunden"
                $ warning "Nicht Gefunden"
            Just (filename, content) -> do
              let mime = Static.mimeTypes Static.defaultOptions (Text.unpack filename)

              send $
                responseLBS
                  status200
                  [("Content-Type", mime)]
                  (BL.fromChunks [content])
    _ -> do
      next req send
