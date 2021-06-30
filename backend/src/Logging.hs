module Logging (withLogger, log, TimedFastLogger, Log) where

import System.Log.FastLogger
import Prelude hiding (log)
import Data.Text

type Log = Text -> IO ()

log :: (ToLogStr s) => TimedFastLogger -> s -> IO ()
log logger msg = logger (\time -> toLogStr (show time) <> " " <> toLogStr msg <> "\n")

withLogger :: (TimedFastLogger -> IO b) -> IO b
withLogger f = do
  formattedTime <- newTimeCache simpleTimeFormat
  withTimedFastLogger formattedTime (LogStdout defaultBufSize) f
