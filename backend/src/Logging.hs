module Logging (withKatip, middleware) where

-- import qualified Network.Wai.Header as Wai

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Katip as K
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai as Wai
import qualified System.IO
import Text.Printf (printf)
import qualified Wai

withKatip :: K.Severity -> K.Namespace -> K.Environment -> K.KatipContextT IO b -> IO b
withKatip minLevel namespace env f = do
  handleScribe <-
    K.mkHandleScribe
      K.ColorIfTerminal
      System.IO.stdout
      (K.permitItem minLevel)
      K.V2

  let makeLogEnv =
        K.registerScribe
          "stdout"
          handleScribe
          K.defaultScribeSettings
          =<< K.initLogEnv namespace env

  bracket makeLogEnv K.closeScribes $ \le -> do
    let initialContext = ()
    let initialNamespace = "server"

    K.runKatipContextT le initialContext initialNamespace f

middleware ::
  ( MonadIO m,
    K.KatipContext m,
    MonadReader env m,
    App.HasRequestIdVaultKey env
  ) =>
  Wai.MiddlewareT m
middleware nextApp req send = do
  now <- liftIO $ Time.getCurrentTime

  let vault = Wai.vault req
  vaultKey <- asks App.getRequestIdVaultKey
  let requestId = maybe "" (T.pack . show) $ Vault.lookup vaultKey vault

  nextApp req $ \response -> do
    afterResponse <- liftIO $ Time.getCurrentTime

    let status = statusCode $ Wai.responseStatus response
        -- This doesn't work. I guess it's because I'm not setting this header.
        -- responseSize = Wai.contentLength (Wai.responseHeaders response)
        duration = Time.nominalDiffTimeToSeconds $ afterResponse `Time.diffUTCTime` now

    K.katipAddNamespace "request_logger" $ do
      K.katipAddContext
        ( (K.sl "time" now)
            -- <> (K.sl "response_size" responseSize)
            <> (K.sl "status" status)
            -- Just give me this as seconds please. Why is this so hard.
            <> (K.sl "duration" (printf "%.3f" (realToFrac duration :: Double) :: String))
            <> (K.sl "request_id" requestId)
        )
        $ K.logLocM K.InfoS "request"

    send response
