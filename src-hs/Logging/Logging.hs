{-# LANGUAGE OverloadedStrings #-}

module Logging.Logging (log, TimedFastLogger) where

import System.Log.FastLogger
import Prelude hiding (log)

log :: (ToLogStr s) => TimedFastLogger -> s -> IO ()
log logger msg = logger (\time -> toLogStr (show time) <> " " <> (toLogStr msg) <> "\n")
