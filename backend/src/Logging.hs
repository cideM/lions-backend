module Logging (withKatip) where

import Control.Exception.Safe
import qualified Katip as K
import qualified System.IO

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
    let initialNamespace = "main"

    K.runKatipContextT le initialContext initialNamespace f
