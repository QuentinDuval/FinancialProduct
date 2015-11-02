{-# LANGUAGE RecordWildCards #-}
module Listed.SimpleOption (
    OptionInfo(..),
    create
) where

import Data.Monoid((<>))
import Flow
import Observable
import Observable.FinancialProduct
import Utils.Monad
import Utils.Time



-- | Test financial product to simple option products

data OptionInfo = OptionInfo {
      premium   :: FinProduct
    , maturity  :: Shifter
    , strike    :: Double
    , quantity  :: ObsQuantity
    , buyInstr  :: String
    , sellInstr :: String
    }

create :: OptionInfo -> FinDate -> FinProduct
create OptionInfo{..} t1 = premium <> opt
    where
        t2  = t1 `addDay` maturity
        val = stockRate buyInstr sellInstr
        opt = ifThen (val t2 .>. cst strike) $
                scale quantity $ mconcat [
                    send $ trn 1      t2 buyInstr,
                    give $ trn strike t2 sellInstr]

