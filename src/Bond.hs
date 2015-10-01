{-# LANGUAGE RecordWildCards #-}
module Bond (
    BondInfo(..),
    PeriodInfo(..),
    buy,
    sell,
) where

import Data.Monoid((<>))
import Data.Time
import EvalMonad
import FinProduct
import Flow



-- | Test financial product to build bond products
-- | Public:

data BondInfo = BondInfo {
      nominal       :: Double
    , rate          :: Quantity
    , currency      :: String
    }

data PeriodInfo = PeriodInfo {
      startDate     :: FlowDate
    , gap           :: Integer  -- ^ in days
    , periodCount   :: Integer
    } deriving(Show, Eq, Ord)


buy :: BondInfo -> PeriodInfo -> FinProduct
buy = create

sell :: BondInfo -> PeriodInfo -> FinProduct
sell bi = create bi { nominal = -1 * nominal bi }


-- | Private:

periodDate :: PeriodInfo -> Integer -> FlowDate
periodDate PeriodInfo{..} n = startDate { utctDay = addDays (n * gap) (utctDay startDate) }

lastDate :: PeriodInfo -> FlowDate
lastDate p = periodDate p (periodCount p)

midDates :: PeriodInfo -> [FlowDate]
midDates p = [periodDate p i | i <- [1 .. periodCount p - 1]]

create :: BondInfo -> PeriodInfo -> FinProduct
create BondInfo{..} p =
    let initFlow = trn nominal (startDate p) currency
        lastFlow = trn (-1 * nominal) (lastDate p) currency
        midFlows = scale rate <$> [trn (-1 * nominal) d currency | d <- midDates p]
    in initFlow <> mconcat midFlows <> lastFlow


