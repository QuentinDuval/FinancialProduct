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
    let t2 = t1 `addDay` div maturity 2
        t3 = t1 `addDay` maturity
        options t = fmap (simpleOptionBody t) bodies
        evalRef = buyInstr (head bodies)
    in premium <>
        bestOfWith (zip (options t2) (options t3)) `withEvalOn` (Stock evalRef, t1)

