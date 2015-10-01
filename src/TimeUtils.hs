module TimeUtils where

import Data.Time


-- | Helpers to work on UTC Time

addDay :: (Integral i) => UTCTime -> i -> UTCTime
addDay utc i = utc { utctDay = addDays (fromIntegral i) (utctDay utc) }

