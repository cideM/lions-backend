-- This file has helpers with writing integration tests. The threshold for
-- adding something new should be fairly low so the tests themselves look
-- relatively clean. I can then periodically go through the code in this file
-- here and see if things can be consolidated.

module Helpers
  ( withDB,
    withFormRequest,
    withQueryString,
    as200,
  )
where

import qualified App
import qualified App.Test
import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import Layout (LayoutStub (..))
import Lucid
import Network.HTTP.Types (hContentType, status200)
import qualified Network.Wai as Wai
import Network.Wai.Test
import qualified System.Directory
import System.FilePath ((</>))
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

-- Essentially the App env but without AWS.
withTestEnv :: App.Environment -> _
withTestEnv env f = do
  tempDir <- System.Directory.getTemporaryDirectory
  let storageDir = tempDir </> "lions_tests_event_storage"

  withDB $ \conn -> do
    ctx <- K.getKatipContext
    ns <- K.getKatipNamespace
    logEnv <- K.getLogEnv

    let env =
          App.Test.Env
            { envDatabaseConnection = conn,
              envEnvironment = env,
              envScryptSignerKey = signerKey,
              envScryptSaltSeparator = saltSep,
              envEventAttachmentStorageDir = storageDir,
              envSessionDataVaultKey = sessionDataVaultKey,
              envRequestIdVaultKey = requestIdVaultKey,
              envSessionEncryptionKey = sessionKey,
              envLogNamespace = ns,
              envLogContext = ctx,
              envLogEnv = logEnv
            }
     in undefined
  where
    signerKey :: B.ByteString
    signerKey = "jxspr8Ki0RYycVU8zykbdLGjFQ3McFUH0uiiTvC8pVMXAn210wjLNmdZJzxUECKbm0QsEmYUSDzZvpjeJ9WmXA=="

    saltSep :: B.ByteString
    saltSep = "Bw=="
