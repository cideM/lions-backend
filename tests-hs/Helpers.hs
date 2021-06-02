{-# LANGUAGE OverloadedStrings #-}

module Helpers (withDB, withFormRequest, as200) where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
import Lucid
import Network.HTTP.Types (hContentType, status200)
import qualified Network.Wai as Wai
import Network.Wai.Test
import System.FilePattern.Directory

-- Run a computation with an SQLite memory DB that has all migrations applied
-- to it
withDB :: (SQLite.Connection -> IO b) -> IO b
withDB f = do
  migrations <- getDirectoryFiles "migrations" ["*.up.sql"]
  SQLite.withConnection
    ":memory:"
    ( \conn -> do
        forM_ migrations (\m -> readFile ("./migrations/" <> m) >>= SQLite.execute_ conn . SQLite.Query . T.pack)
        f conn
    )

as200 = Wai.responseLBS status200 [("Content-Type", "text/html; charset=UTF-8")] . renderBS

withFormRequest ::
  LB.ByteString ->
  (Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived) ->
  IO SResponse
withFormRequest body handler =
  let req = defaultRequest {Wai.requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]}
      session = srequest $ SRequest req body
   in (runSession session $ \r send -> handler r send)

-- lazy BS to strict T
lb2st :: LB.ByteString -> T.Text
lb2st = T.decodeUtf8 . B.concat . LB.toChunks
