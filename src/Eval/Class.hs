{-# LANGUAGE MultiParamTypeClasses #-}
module Eval.Class where

import Eval.MarketData
import Eval.Result
import Utils.Time


-- | Eval monad contract

class IMonadEval evalMonad where
    getStock :: Monad m => String -> FinDate -> evalMonad m Double
    getRate  :: Monad m => String -> FinDate -> evalMonad m Double


-- | Market data access point

type Access m key res = key -> FinDate -> m (Result res)

class (Monad m) => MarketDataAccess m a where
    stockValue :: a -> Access m Stock Double
    rateValue  :: a -> Access m Rate Double

