{-# LANGUAGE RecordWildCards #-}
module Listed.BestOfOption (
    module Listed.Option.Core,
    bestOfOption
) where


import Data.Monoid((<>))
import Listed.Option.Core
import Observable
import Payoff
import Utils.Monad
import Utils.Time


-- |

--data BestOfOptionInfo = BestOfOptionInfo {
--      premium   :: FinProduct
--    , maturity  :: Shifter
--    , strike    :: Double
--    , quantity  :: ObsQuantity
--    , buyInstr  :: String
--    , sellInstr :: String
--} deriving (Show, Read, Eq, Ord)
--
--
--simpleOption :: OptionInfo -> FinDate -> FinProduct
--simpleOption OptionInfo{..} t1 = premium <> opt
--    where
--        t2  = t1 `addDay` maturity
--        val = stock buyInstr t2 / stock sellInstr t2
--        opt = ifThen (val .>. cst strike) $
--                scale quantity $ mconcat [
--                    send $ trn 1      t2 buyInstr,
--                    give $ trn strike t2 sellInstr]



