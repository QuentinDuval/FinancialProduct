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
bestOfOption (CompositeOption OptionHeader{..} bodies) t1
    = premium <> bestOf (fmap (simpleOptionBody t1) bodies) `withEvalOn` (Stock "USD", t1)

