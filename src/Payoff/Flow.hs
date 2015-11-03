{-# LANGUAGE RecordWildCards #-}
module Payoff.Flow where

import EvalProd
import MarketData
import Numeric
import Observable
import Utils.Time


-- | Data structure to describe the production of financial products

data Flow = Flow
    { flow      :: Double
    , date      :: FinDate
    , flowInstr :: Stock }
    deriving (Eq, Ord)


-- | Custom instances

instance Show Flow where
    show Flow{ flow = f, date = d, flowInstr = (Stock s) } =
        "Flow { value = "   ++ showFFloat (Just 2) f ""
        ++ ", date = "      ++ show d
        ++ ", instr = "     ++ show s
        ++ " }"


-- | Utils function

overFlow :: (Double -> Double) -> Flow -> Flow
overFlow modifier f = f { flow = modifier (flow f) }


-- | Evaluation function

convert :: (Monad m) => Stock -> Flow -> EvalProd m Flow
convert newInstr f@Flow{..} = do
    v1 <- evalObs $ StockObs (stockLabel flowInstr) date
    v2 <- evalObs $ StockObs (stockLabel newInstr) date
    return $ f { flow = v1 / v2 * flow, flowInstr = newInstr }

shiftDate :: (Monad m) => FinDate -> Flow -> EvalProd m Flow
shiftDate = undefined -- TODO


