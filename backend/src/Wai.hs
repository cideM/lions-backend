module Wai (parseQueryParams, parseParams, paramsToMap) where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Network.Wai as Wai
import Network.Wai.Parse (defaultParseRequestBodyOptions, lbsBackEnd, parseRequestBodyEx)

paramsToMap :: [(ByteString, ByteString)] -> Map Text Text
paramsToMap = Map.fromList . map (bimap decodeUtf8 decodeUtf8)

queryStringToMap :: [(ByteString, Maybe ByteString)] -> Map Text Text
queryStringToMap = Map.fromList . map (bimap decodeUtf8 decodeUtf8) . foldl' f []
  where
    f xs (a, Just b) = (a, b) : xs
    f xs _ = xs

parseParams :: Wai.Request -> IO (Map Text Text)
parseParams req = do
  body <- parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd req
  return . paramsToMap $ fst body

parseQueryParams :: Wai.Request -> Map Text Text
parseQueryParams = queryStringToMap . Wai.queryString
