{-# LANGUAGE TemplateHaskell #-}
module Flow where

import Control.Lens
import Data.Time


-- | Data structure to describe the production of financial products

type FlowDate = UTCTime

data Flow = Flow
    { _flow     :: Double
    , _date     :: FlowDate
    , _flowCurr :: String }
    deriving (Eq, Ord)

makeLenses ''Flow


-- | Custom instances

instance Show Flow where
    show f =
        "Flow { value = "   ++ show (_flow f)
        ++ ", date = "      ++ show (utctDay (_date f))
        ++ ", currency = "  ++ show (_flowCurr f)
        ++ " }"

