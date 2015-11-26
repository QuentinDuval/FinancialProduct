{-# LANGUAGE RecordWildCards #-}
module Listed.Bond (
    BondInfo(..),
    PeriodInfo(..),
    buyBond,
    sellBond,
) where

import Data.Monoid((<>))
import Observable
import Payoff
import Utils.Time


-- | Test financial product to build bond products

data BondInfo = BondInfo {
      nominal       :: Double
    , couponRate    :: FinDate -> ObsQuantity
    , currency      :: String
}

data PeriodInfo = PeriodInfo {
      startDate     :: FinDate
    , period        :: Shifter
    , periodCount   :: Integer
} deriving (Show, Read, Eq, Ord)


buyBond, sellBond :: BondInfo -> PeriodInfo -> FinProduct
buyBond        = makeBond
sellBond bi pi = give (makeBond bi pi)


-- | Private:

lastDate :: PeriodInfo -> FinDate
lastDate PeriodInfo{..} = addDay startDate (period * periodCount)

midDates :: PeriodInfo -> [FinDate]
midDates PeriodInfo{..} = addDay startDate . (period *) <$> [1..periodCount-1]

makeBond :: BondInfo -> PeriodInfo -> FinProduct
makeBond BondInfo{..} p =
    let initFlow = recv nominal (startDate p) currency
        lastFlow = send nominal (lastDate p) currency
        midFlows = [scale (couponRate d) (send nominal d currency) | d <- midDates p]
    in initFlow <> mconcat midFlows <> lastFlow
