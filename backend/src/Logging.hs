module Logging (withKatip) where

import Control.Exception.Safe
import Control.Monad.IO.Class (liftIO)
import qualified Katip as K
import qualified System.IO

-- TODO: MyApp etc
withKatip :: K.KatipContextT IO b -> IO b
withKatip f = do
  handleScribe <- liftIO $ K.mkHandleScribe K.ColorIfTerminal System.IO.stdout (K.permitItem K.InfoS) K.V2

  let makeLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings =<< K.initLogEnv "MyApp" "production"

  bracket makeLogEnv K.closeScribes $ \le -> do
    let initialContext = ()
    let initialNamespace = "main"

    K.runKatipContextT le initialContext initialNamespace f
