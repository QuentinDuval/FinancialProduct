{-# LANGUAGE RecordWildCards #-}
module Listed.Option.BestOf (
    module Listed.Option.Core,
    bestOfOption,
) where

import Data.Monoid((<>))
import Listed.Option.Core
import Observable
import Payoff
import Utils.Monad
import Utils.Time


bestOfOption :: CompositeOption -> FinDate -> FinProduct
bestOfOption (CompositeOption OptionHeader{..} bodies) t1
    = undefined
--    = premium <> opt
--    where
--        t2  = t1 `addDay` maturity
--        val = stock buyInstr t2 / stock sellInstr t2
--        opt = ifThen (val .>. cst strike) $
--                scale quantity $ mconcat [
--                    send $ trn 1      t2 buyInstr,
--                    give $ trn strike t2 sellInstr]

