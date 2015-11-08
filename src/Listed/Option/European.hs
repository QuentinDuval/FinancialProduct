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
europeanOption (SimpleOption OptionHeader{..} OptionBody{..}) t1 = premium <> opt
    where
        t2  = t1 `addDay` maturity
        val = stock buyInstr t2 / stock sellInstr t2
        opt = ifThen (val .>. cst strike) $
                scale quantity $ mconcat [
                    send $ trn 1      t2 buyInstr,
                    give $ trn strike t2 sellInstr]

