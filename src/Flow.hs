{-# LANGUAGE RecordWildCards #-}
module Flow where

import EvalMonad
import MarketData
import Numeric
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
        ++ ", date = "      ++ show (utctDay d)
        ++ ", instr = "     ++ show s
        ++ " }"


-- | Utils function

overFlow :: (Double -> Double) -> Flow -> Flow
overFlow modifier f = f { flow = modifier (flow f) }


-- | Evaluation function

convert :: Stock -> Flow -> EvalMonad Flow
convert newInstr f@Flow{..} = do
    v1 <- evalVar flowInstr date
    v2 <- evalVar newInstr date
    return $ f { flow = v1 / v2 * flow, flowInstr = newInstr }

