module Eval.Class where

import Control.Applicative
import Eval.MarketData
import Utils.Time


-- | Eval monad contract

class (Monad m, Alternative m) => IMarketEval m where
    getStock :: String -> FinDate -> m Double
    getRate  :: String -> FinDate -> m Double


