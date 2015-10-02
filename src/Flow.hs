{-# LANGUAGE TemplateHaskell #-}
module Flow where

import Control.Lens
import Numeric
import TimeUtils


-- | Data structure to describe the production of financial products

data Flow = Flow
    { _flow     :: Double
    , _date     :: FinDate
    , _flowCurr :: String }
    deriving (Eq, Ord)

makeLenses ''Flow


-- | Custom instances

instance Show Flow where
    show f =
        "Flow { value = "   ++ showFFloat (Just 2) (_flow f) ""
        ++ ", date = "      ++ show (utctDay (_date f))
        ++ ", currency = "  ++ show (_flowCurr f)
        ++ " }"

