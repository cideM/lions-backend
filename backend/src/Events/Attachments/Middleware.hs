module Events.Attachments.Middleware (middleware) where

import qualified Data.Text as Text
import Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Static
import qualified UnliftIO
import qualified Wai

-- This middleware intercepts requests to event attachments and rewrites them
-- so that, relative to the event attachments storage location, the filepath is
-- correct. It turns "/events/2/foo.pdf" into "/2/foo.pdf"
middleware :: (UnliftIO.MonadUnliftIO m) => String -> Wai.MiddlewareT m
middleware storageDir =
  Wai.liftMiddleware (Static.staticPolicy (Static.policy rewriteEvents >-> Static.addBase storageDir))
  where
    rewriteEvents :: String -> Maybe String
    rewriteEvents s =
      let t = Text.pack s
       in if "events/" `Text.isPrefixOf` t
            then Just (Text.unpack $ Text.replace "events/" "" t)
            else Nothing
