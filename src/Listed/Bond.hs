{-# LANGUAGE RecordWildCards #-}
module Listed.Bond (
    BondInfo(..),
    PeriodInfo(..),
    buy,
    sell,
) where

import Data.Monoid((<>))
import Observable
import Payoff
import Utils.Time



-- | Test financial product to build bond products
-- | Public:

data BondInfo = BondInfo {
      nominal       :: Double
    , couponRate    :: FinDate -> ObsQuantity
    , currency      :: String
    }

data PeriodInfo = PeriodInfo {
      startDate     :: FinDate
    , period        :: Shifter
    , periodCount   :: Integer
    } deriving(Show, Eq, Ord)


buy :: BondInfo -> PeriodInfo -> FinProduct
buy = create

sell :: BondInfo -> PeriodInfo -> FinProduct
sell bi pi = give (create bi pi)


-- | Private:

lastDate :: PeriodInfo -> FinDate
lastDate PeriodInfo{..} = addDay startDate (period * periodCount)

midDates :: PeriodInfo -> [FinDate]
midDates PeriodInfo{..} = addDay startDate . (period *) <$> [1..periodCount-1]

create :: BondInfo -> PeriodInfo -> FinProduct
create BondInfo{..} p =
    let repayment = -1 * nominal
        initFlow = trn nominal (startDate p) currency
        lastFlow = trn repayment (lastDate p) currency
        midFlows = [scale (couponRate d) (trn repayment d currency) | d <- midDates p]
    in initFlow <> mconcat midFlows <> lastFlow
