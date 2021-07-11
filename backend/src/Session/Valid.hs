module Session.Valid where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Session.Types
import User.Types (UserId (..))
import Time (timeDaysFromNow)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai.Session (genSessionId)

create :: (MonadIO m) => UserId -> m ValidSession
create uid = do
  expires <- liftIO $ timeDaysFromNow 30
  sessionid <- liftIO $ decodeUtf8 <$> genSessionId
  return . ValidSession $ Session (SessionId sessionid) expires uid
