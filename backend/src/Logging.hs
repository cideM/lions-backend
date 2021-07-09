module Logging (withKatip, withLogger, log, TimedFastLogger, Log) where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text
import Data.Text (Text)
import qualified Katip as K
import qualified System.IO
import System.Log.FastLogger
import Prelude hiding (log)

type Log = Text -> IO ()

log :: (ToLogStr s) => TimedFastLogger -> s -> IO ()
log logger msg = logger (\time -> toLogStr (show time) <> " " <> toLogStr msg <> "\n")

withLogger :: (TimedFastLogger -> IO b) -> IO b
withLogger f = do
  formattedTime <- newTimeCache simpleTimeFormat
  withTimedFastLogger formattedTime (LogStdout defaultBufSize) f

-- TODO: MyApp etc
withKatip :: K.KatipContextT IO b -> IO b
withKatip f = do
  handleScribe <- liftIO $ K.mkHandleScribe K.ColorIfTerminal System.IO.stdout (K.permitItem K.InfoS) K.V2

  let makeLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings =<< K.initLogEnv "MyApp" "production"

  bracket makeLogEnv K.closeScribes $ \le -> do
    let initialContext = ()
    let initialNamespace = "main"

    K.runKatipContextT le initialContext initialNamespace f
