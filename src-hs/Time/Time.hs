module Time.Time (timeDaysFromNow) where

import qualified Data.Time as Time
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (foldl')

timeDaysFromNow :: (MonadIO m) => Int -> m Time.UTCTime
timeDaysFromNow days = do
  now <- liftIO Time.getCurrentTime
  let thirtyDays = replicate days Time.nominalDay
  return $ foldl' (flip Time.addUTCTime) now thirtyDays

