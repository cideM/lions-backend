module Logging (withLogger, log, TimedFastLogger) where

import System.Log.FastLogger
import Prelude hiding (log)

log :: (ToLogStr s) => TimedFastLogger -> s -> IO ()
log logger msg = logger (\time -> toLogStr (show time) <> " " <> toLogStr msg <> "\n")

withLogger :: (TimedFastLogger -> IO b) -> IO b
withLogger f = do
  formattedTime <- newTimeCache simpleTimeFormat
  withTimedFastLogger formattedTime (LogStdout defaultBufSize) f
