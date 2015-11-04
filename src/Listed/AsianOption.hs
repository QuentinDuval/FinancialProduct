{-# LANGUAGE RecordWildCards #-}
module Listed.AsianOption(
    AsianOptionInfo(..),
    asianOption,
) where

import Data.Monoid((<>))
import Listed.SimpleOption
import Eval.MarketData
import Observable
import Payoff
import Utils.Monad
import Utils.Time



-- | Test financial product to simple option products

data AsianOptionInfo = AsianOptionInfo {
      optInfo   :: OptionInfo
    , evalGap   :: Shifter
} deriving (Show, Read, Eq, Ord)


asianOption :: AsianOptionInfo -> FinDate -> FinProduct
asianOption AsianOptionInfo { optInfo = OptionInfo{..}, .. } t1
    = premium <> opt
    where
        t2  = t1 `addDay` maturity
        ts  = takeWhile (< t2) $ iterate (`addDay` evalGap) t1
        val = averageStockRate buyInstr sellInstr ts
        opt = ifThen (val .>. cst strike) $
                scale quantity $ mconcat [
                    send $ trn 1      t2 buyInstr,
                    give $ trn strike t2 sellInstr]


