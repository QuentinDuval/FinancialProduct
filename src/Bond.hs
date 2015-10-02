{-# LANGUAGE RecordWildCards #-}
module Bond (
    BondInfo(..),
    PeriodInfo(..),
    buy,
    sell,
) where

import Data.Monoid((<>))
import EvalMonad
import FinProduct
import Flow
import TimeUtils



-- | Test financial product to build bond products
-- | Public:

data BondInfo = BondInfo {
      nominal       :: Double
    , couponRate    :: FinDate -> Quantity
    , currency      :: String
    }

data PeriodInfo = PeriodInfo {
      startDate     :: FinDate
    , gap           :: Integer  -- ^ in days
    , periodCount   :: Integer
    } deriving(Show, Eq, Ord)


buy :: BondInfo -> PeriodInfo -> FinProduct
buy = create

sell :: BondInfo -> PeriodInfo -> FinProduct
sell bi = create bi { nominal = -1 * nominal bi }


-- | Private:

lastDate :: PeriodInfo -> FinDate
lastDate PeriodInfo{..} = addDay startDate (gap * periodCount)

midDates :: PeriodInfo -> [FinDate]
midDates PeriodInfo{..} = addDay startDate . (gap *) <$> [1..periodCount-1]

create :: BondInfo -> PeriodInfo -> FinProduct
create BondInfo{..} p =
    let repayment = -1 * nominal
        initFlow = trn nominal (startDate p) currency
        lastFlow = trn repayment (lastDate p) currency
        midFlows = [scale (couponRate d) (trn repayment d currency) | d <- midDates p]
    in initFlow <> mconcat midFlows <> lastFlow


