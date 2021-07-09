module Wai.Class where

import Control.Monad.IO.Class (liftIO)
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived)
import qualified UnliftIO

-- Shamelessly stolen from
-- https://github.com/EarnestResearch/honeycomb-wai-haskell/blob/216ec3bf7330ed5a8032dd166d2a36820239b343/src/Network/Wai/Honeycomb.hs

type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived

type MiddlewareT m = ApplicationT m -> ApplicationT m

liftApplication :: (UnliftIO.MonadUnliftIO m) => Application -> ApplicationT m
liftApplication app req respond =
  UnliftIO.withRunInIO $ \runInIO -> liftIO $ app req (runInIO . respond)

liftMiddleware :: (UnliftIO.MonadUnliftIO m) => Middleware -> MiddlewareT m
liftMiddleware mid app req respond = do
  app' <- runApplicationT app
  UnliftIO.withRunInIO $ \runInIO -> mid app' req (runInIO . respond)

runApplicationT :: (UnliftIO.MonadUnliftIO m) => ApplicationT m -> m Application
runApplicationT app =
  UnliftIO.withRunInIO $ \runInIO ->
    pure $ \req respond ->
      runInIO $ app req (liftIO . respond)

runMiddlewareT :: (UnliftIO.MonadUnliftIO m) => MiddlewareT m -> m Middleware
runMiddlewareT mid =
  UnliftIO.withRunInIO $ \runInIO ->
    pure $ \app req respond -> do
      app' <- runInIO . runApplicationT . mid $ liftApplication app
      app' req respond
