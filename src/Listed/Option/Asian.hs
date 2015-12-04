{-# LANGUAGE RecordWildCards #-}
module Listed.Option.Asian (
    module Listed.Option.Core,
    asianOption,
) where

import Data.Monoid((<>))
import Listed.Option.Core
import Eval.MarketData
import Observable
import Payoff
import Utils.Monad
import Utils.Time


type EvalGap = Shifter

asianOption :: SimpleOption -> EvalGap -> FinDate -> FinProduct
asianOption (SimpleOption OptionHeader{..} OptionBody{..}) evalGap t1
    = premium <> opt
    where
        t2  = t1 `addDay` maturity
        ts  = takeWhile (< t2) $ iterate (`addDay` evalGap) t1
        val = averageStockRate buyInstr sellInstr ts
        opt = ifThen (val .>. cst strike) $
                scale quantity $ mconcat [
                    recv 1      t2 buyInstr,
                    send strike t2 sellInstr]

