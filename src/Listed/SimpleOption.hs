{-# LANGUAGE RecordWildCards #-}
module Listed.SimpleOption (
    module Listed.Option.Core,
    simpleOption
) where

import Data.Monoid((<>))
import Listed.Option.Core
import Observable
import Payoff
import Utils.Monad
import Utils.Time



-- |

simpleOption :: SimpleOption -> FinDate -> FinProduct
simpleOption (SimpleOption OptionHeader{..} OptionBody{..}) t1 = premium <> opt
    where
        t2  = t1 `addDay` maturity
        val = stock buyInstr t2 / stock sellInstr t2
        opt = ifThen (val .>. cst strike) $
                scale quantity $ mconcat [
                    send $ trn 1      t2 buyInstr,
                    give $ trn strike t2 sellInstr]

