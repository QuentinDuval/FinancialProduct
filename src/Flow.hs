module Flow where

import Numeric
import Utils.Time


-- | Data structure to describe the production of financial products

data Flow = Flow
    { flow     :: Double
    , date     :: FinDate
    , flowCurr :: String }
    deriving (Eq, Ord)


-- | Custom instances

instance Show Flow where
    show f =
        "Flow { value = "   ++ showFFloat (Just 2) (flow f) ""
        ++ ", date = "      ++ show (utctDay (date f))
        ++ ", currency = "  ++ show (flowCurr f)
        ++ " }"


-- | Utils function

overFlow :: (Double -> Double) -> Flow -> Flow
overFlow modifier f = f { flow = modifier (flow f) }
