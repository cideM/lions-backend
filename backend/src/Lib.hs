{-# LANGUAGE TemplateHaskell #-}

module Lib (main) where

import qualified App
import Control.Monad.IO.Class (liftIO)
import Katip
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import qualified Server
import Prelude hiding (id)

main :: IO ()
main = do
  let port = 3000

  App.withAppEnv $ \env -> do
    katipAddContext (sl "port" port) $ do
      $(logTM) InfoS "listening..."

      let settings = setPort port $ setHost "0.0.0.0" defaultSettings

      let runApp = flip App.unApp env

      liftIO $
        runSettings
          settings
          (\request send -> runApp $ Server.app request (liftIO . send))
