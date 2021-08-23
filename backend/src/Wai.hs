module Wai
  ( parseQueryParams,
    parseParams,
    paramsToMap,
    ApplicationT,
    MiddlewareT,
    liftApplication,
    liftMiddleware,
    runApplicationT,
    runMiddlewareT,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived)
import qualified Network.Wai as Wai
import Network.Wai.Parse (defaultParseRequestBodyOptions, lbsBackEnd, parseRequestBodyEx)
import qualified UnliftIO

paramsToMap :: [(ByteString, ByteString)] -> Map Text Text
paramsToMap = Map.fromList . map (bimap decodeUtf8 decodeUtf8)

queryStringToMap :: [(ByteString, Maybe ByteString)] -> Map Text Text
queryStringToMap = Map.fromList . map (bimap decodeUtf8 decodeUtf8) . foldl' f []
  where
    f xs (a, Just b) = (a, b) : xs
    f xs _ = xs

-- This function will overwrite values with the same key which is definitely a
-- feature in HTML forms. Keep that in mind.
parseParams :: Wai.Request -> IO (Map Text Text)
parseParams req = do
  body <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd req
  return . paramsToMap $ fst body

parseQueryParams :: Wai.Request -> Map Text Text
parseQueryParams = queryStringToMap . Wai.queryString

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
