{-# LANGUAGE MultiParamTypeClasses #-}
module Eval.MarketData where

import Eval.Result
import Utils.Time


-- | Market data definition

type StockId    = String
type RateId     = String

newtype Stock = Stock { stockLabel :: StockId }
    deriving (Show, Read, Eq, Ord)

newtype Rate = Rate { rateLabel  :: RateId }
    deriving (Show, Read, Eq, Ord)


-- | Market data access point

type Access m key res = key -> FinDate -> m (Result res)

class (Monad m) => MarketDataAccess m a where
    stockValue :: a -> Access m Stock Double
    rateValue  :: a -> Access m Rate Double

