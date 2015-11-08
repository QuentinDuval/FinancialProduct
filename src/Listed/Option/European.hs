{-# LANGUAGE RecordWildCards #-}
module Listed.Option.European (
    module Listed.Option.Core,
    europeanOption,
) where


import Data.Monoid((<>))
import Listed.Option.Core
import Observable
import Payoff
import Utils.Monad
import Utils.Time


europeanOption :: SimpleOption -> FinDate -> FinProduct
europeanOption o@(SimpleOption OptionHeader{..} optBody) t1
    = premium <> simpleOptionBody (t1 `addDay` maturity) optBody

