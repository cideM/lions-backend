module Events.Attachments.Middleware (middleware) where

import qualified Data.Text as Text
import Network.Wai.Middleware.Static ((>->))
import qualified Network.Wai.Middleware.Static as Static
import qualified UnliftIO
import qualified Wai.Class as Wai

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
