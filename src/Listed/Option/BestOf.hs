{-# LANGUAGE RecordWildCards #-}
module Listed.Option.BestOf (
    module Listed.Option.Core,
    bestOfOption,
) where

import Data.Monoid((<>))
import Eval
import Listed.Option.Core
import Observable
import Payoff
import Utils.Monad
import Utils.Time



bestOfOption :: CompositeOption -> FinDate -> FinProduct
bestOfOption (CompositeOption _ []) _ = Empty
bestOfOption (CompositeOption OptionHeader{..} bodies) t1 =
    let counts  = reverse [1 .. length bodies]
        shifts  = [div maturity (fromIntegral i) | i <- counts]
        options = fmap (simpleOptionBody t1) bodies
        evalRef = sellInstr (head bodies)
    in premium <>
        cascadingBestsOf (zip counts shifts) options `withEvalOn` (Stock evalRef, t1)


