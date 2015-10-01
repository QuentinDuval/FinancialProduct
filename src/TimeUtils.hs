module TimeUtils (
    module TimeUtils,
    module Data.Time
) where

import Data.Time


-- | Type definitions

type FinDate = UTCTime
type Shifter = Integer


-- | Helpers to work on UTC Time

addDay :: FinDate -> Shifter -> FinDate
addDay utc shifter = utc { utctDay = addDays shifter (utctDay utc) }


-- | Time to seconds

toDayCount :: FinDate -> Double
toDayCount = fromIntegral . toModifiedJulianDay . localDay . utcToLocalTime utc

