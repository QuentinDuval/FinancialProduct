module TimeUtils where

import Data.Time



-- | Type definitions

type FinDate = UTCTime
type Shifter = Integer


-- | Helpers to work on UTC Time

addDay :: FinDate -> Shifter -> FinDate
addDay utc shifter = utc { utctDay = addDays shifter (utctDay utc) }

