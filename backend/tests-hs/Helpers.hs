-- This file has helpers with writing integration tests. The threshold for
-- adding something new should be fairly low so the tests themselves look
-- relatively clean. I can then periodically go through the code in this file
-- here and see if things can be consolidated.

module Helpers
  ( withDB,
    withFormRequest,
    withQueryString,
    withoutLogging,
    as200,
  )
where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import Lucid
import Layout (LayoutStub(..))
import Network.HTTP.Types (hContentType, status200)
import qualified Network.Wai as Wai
import Network.Wai.Test
import qualified Logging
import System.FilePattern.Directory
import System.Log.FastLogger
  ( LogType' (..),
    TimedFastLogger,
    defaultBufSize,
    newTimeCache,
    simpleTimeFormat,
    withTimedFastLogger,
  )

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

as200 :: LayoutStub -> Wai.Response
as200 = Wai.responseLBS status200 [("Content-Type", "text/html; charset=UTF-8")] . renderBS . layoutStubContent

withQueryString ::
  B.ByteString ->
  (Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived) ->
  IO SResponse
withQueryString qs handler =
  let req = setPath defaultRequest qs
      session = srequest $ SRequest req ""
   in (runSession session $ \r send -> handler r send)

withFormRequest ::
  LB.ByteString ->
  (Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived) ->
  IO SResponse
withFormRequest body handler =
  let req = defaultRequest {Wai.requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]}
      session = srequest $ SRequest req body
   in (runSession session $ \r send -> handler r send)

withoutLogging :: (Logging.Log -> IO ()) -> IO ()
withoutLogging = return . const ()

