{-# LANGUAGE MultiParamTypeClasses #-}
module Eval.Class where

import Control.Applicative
import Eval.MarketData
import Eval.Result
import Utils.Time


-- | Eval monad contract

class (Monad m, Alternative m) => IMarketEval m where
    getStock :: String -> FinDate -> m Double
    getRate  :: String -> FinDate -> m Double


-- | Market data access point

type Access m key res = key -> FinDate -> m (Result res)

class (Monad m) => MarketDataAccess m a where
    stockValue :: a -> Access m Stock Double
    rateValue  :: a -> Access m Rate Double

