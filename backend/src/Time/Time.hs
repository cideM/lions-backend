module Time.Time (timeDaysFromNow) where

import Data.List (foldl')
import qualified Data.Time as Time

timeDaysFromNow :: Int -> IO Time.UTCTime
timeDaysFromNow days = do
  now <- Time.getCurrentTime
  let thirtyDays = replicate days Time.nominalDay
  return $ foldl' (flip Time.addUTCTime) now thirtyDays
