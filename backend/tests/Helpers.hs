-- This file has helpers with writing integration tests. The threshold for
-- adding something new should be fairly low so the tests themselves look
-- relatively clean. I can then periodically go through the code in this file
-- here and see if things can be consolidated.

module Helpers
  ( withDB,
    withFormRequest,
    withTestEnv,
    withTestEnvProd,
    runSession',
    withRender200,
    withQueryString,
    renderLayoutStub200,
  )
where

import qualified App
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT, withInternalState)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import qualified Katip as K
import Layout (LayoutStub (..))
import qualified Logging
import Lucid
import Network.HTTP.Types (hContentType, status200)
import qualified Network.Wai as Wai
import Network.Wai.Test
import qualified Password.Reset.Mail as Mail
import qualified System.Directory
import System.FilePath ((</>))
import System.FilePattern.Directory
import qualified UnliftIO
import qualified Web.ClientSession as ClientSession

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

renderLayoutStub200 :: LayoutStub -> Wai.Response
renderLayoutStub200 = Wai.responseLBS status200 [("Content-Type", "text/html; charset=UTF-8")] . renderBS . layoutStubContent

-- standing for ‘Network.Wai.Internal.Request
--               -> (Network.Wai.Internal.Response -> App.App App.Env b)
--               -> App.App App.Env b’

withRender200 ::
  (MonadIO m) =>
  (Wai.Request -> m LayoutStub) ->
  Wai.Request ->
  (Wai.Response -> m b) ->
  m b
withRender200 handler req send = handler req >>= send . renderLayoutStub200

withQueryString ::
  B.ByteString ->
  (Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived) ->
  IO SResponse
withQueryString qs handler =
  let req = setPath defaultRequest qs
      session = srequest $ SRequest req ""
   in (runSession session $ \r send -> handler r send)

runSession' ::
  (UnliftIO.MonadUnliftIO m, MonadIO m) =>
  Session b ->
  (Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived) ->
  m b
runSession' session handler =
  UnliftIO.withRunInIO $ \runInIO ->
    runSession session $ \r send -> runInIO $ handler r (liftIO . send)

withFormRequest ::
  (MonadIO m, UnliftIO.MonadUnliftIO m) =>
  LB.ByteString ->
  (Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived) ->
  m SResponse
withFormRequest body handler =
  let req = defaultRequest {Wai.requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]}
      session = srequest $ SRequest req body
   in -- The handler, after getting request and send, has "m Wai.ResponseReceived".
      -- "runInIO" takes this and turns it into "IO Wai.ResponseReceived" since
      -- my App type has IO. "withRunInIO" then reassembles my App monad, and
      -- thus "withFormRequest" returns "m SResponse" where "m" will be my App
      -- monad.
      UnliftIO.withRunInIO $ \runInIO ->
        runSession session $ \r send -> runInIO $ handler r (liftIO . send)

-- This generates a proper environment in which we can run our app, but all the
-- values are hardcoded. Most importantly this takes the randomness out of
-- password hashing. It also doesn't talk to AWS and instead uses an IO Ref to
-- track emails.
withTestEnv :: App.Environment -> (IORef (Maybe Text, Maybe Mail.Mail) -> App.App App.Env a) -> IO a
withTestEnv appEnv f = do
  tempDir <- System.Directory.getTemporaryDirectory

  mailRef <- IORef.newIORef (Nothing, Nothing)
  (_, sessionKey) <- ClientSession.randomKey
  sessionDataVaultKey <- Vault.newKey
  requestIdVaultKey <- Vault.newKey

  runResourceT $
    withInternalState
      ( \internalState -> do
          withDB $ \conn -> do
            Logging.withKatip K.V3 K.DebugS "main" (K.Environment . T.pack $ show appEnv) $ do
              K.katipNoLogging $ do
                ctx <- K.getKatipContext
                ns <- K.getKatipNamespace
                logEnv <- K.getLogEnv

                let env =
                      App.Env
                        { envDatabaseConnection = conn,
                          envEnvironment = appEnv,
                          envScryptSignerKey = signerKey,
                          envScryptSaltSeparator = saltSep,
                          envMail = Mail.sendIoRef mailRef,
                          envInternalState = internalState,
                          -- TODO: Remove from env since not used in tests
                          envPort = 5000,
                          envSessionDataVaultKey = sessionDataVaultKey,
                          envRequestIdVaultKey = requestIdVaultKey,
                          envSessionEncryptionKey = sessionKey,
                          envLogNamespace = ns,
                          envLogContext = ctx,
                          envLogEnv = logEnv
                        }
                 in liftIO $ App.unApp (f mailRef) env
      )
  where
    signerKey :: B.ByteString
    signerKey = "jxspr8Ki0RYycVU8zykbdLGjFQ3McFUH0uiiTvC8pVMXAn210wjLNmdZJzxUECKbm0QsEmYUSDzZvpjeJ9WmXA=="

    saltSep :: B.ByteString
    saltSep = "Bw=="

withTestEnvProd :: (IORef (Maybe Text, Maybe Mail.Mail) -> App.App App.Env a) -> IO a
withTestEnvProd = withTestEnv App.Production
