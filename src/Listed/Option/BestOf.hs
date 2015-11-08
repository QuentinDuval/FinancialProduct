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


-- TODO: In reality, there are several stages of selections. How to model this?
-- TODO: BestOf should take a constructor to package the products selected.
--       BestOf to AllOf is the base, but not limited to that.

bestOfOption :: CompositeOption -> FinDate -> FinProduct
bestOfOption (CompositeOption _ []) _ = Empty
bestOfOption (CompositeOption OptionHeader{..} bodies) t1 =
    let t2 = t1 `addDay` div maturity 2
        t3 = t1 `addDay` maturity
        options t = fmap (simpleOptionBody t) bodies
        evalRef = sellInstr (head bodies)
    in premium <>
        bestOfWith (zip (options t2) (options t3)) `withEvalOn` (Stock evalRef, t1)

