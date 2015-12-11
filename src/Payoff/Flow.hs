{-# LANGUAGE RecordWildCards #-}
module Payoff.Flow (
    Flow(..),
    overFlow,
    roundFlow,
    convert,
    compound,
) where

import Eval
import Numeric
import Observable
import Utils.Time


-- | Data structure to describe the production of financial products

data Flow = Flow
    { flow      :: Double
    , date      :: FinDate
    , flowInstr :: Stock }
    deriving (Eq, Ord)

instance Show Flow where
    show Flow{ flow = f, date = d, flowInstr = (Stock s) } =
        "Flow { value = "   ++ show f --showFFloat (Just 3) f ""
        ++ ", date = "      ++ show d
        ++ ", instr = "     ++ show s
        ++ " }"


-- | Utils function

overFlow :: (Double -> Double) -> Flow -> Flow
overFlow modifier f = f { flow = modifier (flow f) }

roundFlow :: Int -> Flow -> Flow
roundFlow n = overFlow (\f -> fromIntegral (round (f * 10^n)) / 10^n)


-- | Convert a flow from one instrument to another

convert :: (IMarketEval m) => Stock -> Flow -> m Flow
convert newInstr f@Flow{..} = do
    r <- evalStockRate (flowInstr, date) (newInstr, date)
    pure $ f { flow = r * flow, flowInstr = newInstr }


-- | Evaluate the value of a flow at a given date, taking into account the financing rate

compound :: (IMarketEval m) => FinDate -> Flow -> m Flow
compound newDate f@Flow{..} = do
    r <- evalStockRate (flowInstr, newDate) (flowInstr, date)
    pure $ f { flow = r * flow, date = newDate }


-- | Private

evalStockRate :: (IMarketEval m) => (Stock, FinDate) -> (Stock, FinDate) -> m Double
evalStockRate (s1, t1) (s2, t2) = evalObs (stock (stockLabel s1) t1 / stock (stockLabel s2) t2)

