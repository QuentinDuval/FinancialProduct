module Utils.Time (
    module Utils.Time,
    module Data.Time
) where

import Data.Time


-- | Type definitions

type FinDate = Day
type Shifter = Integer


-- | Helpers to work on UTC Time

addDay :: FinDate -> Shifter -> FinDate
addDay utc shifter = addDays shifter utc


-- | Time to seconds

toDayCount :: (Num a) => FinDate -> a
toDayCount = fromIntegral . toModifiedJulianDay

