{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MarketData where

import qualified Data.Map as M
import TimeUtils


-- | Market data set definition

data Stock      = Stock String deriving (Show, Eq, Ord)
data Rate       = Rate  String deriving (Show, Eq, Ord)
type TimeValue  = FinDate -> Double

data MarketData = MarketData
    { stockMap :: M.Map Stock TimeValue
    , rateMap  :: M.Map Rate  TimeValue }


-- | Observable values in the market data

class Observable a b | a -> b where
    observe :: MarketData -> a -> FinDate -> Maybe b

instance Observable Stock Double where
    observe = findInMap . stockMap

instance Observable Rate Double where
    observe = findInMap . rateMap


-- | Helpers to construct a market data set

emptyMds :: MarketData
emptyMds = MarketData M.empty M.empty

initMds :: (Foldable f1, Foldable f2) => f1 (Stock, TimeValue) -> f2 (Rate, TimeValue) -> MarketData
initMds s r = addStocks s (addRates r emptyMds)

addStocks :: (Foldable f) => f (Stock, TimeValue) -> MarketData -> MarketData
addStocks f m = m { stockMap = stockMap m `addToMap` f }

addRates :: (Foldable f) => f (Rate, TimeValue) -> MarketData -> MarketData
addRates f m = m { rateMap = rateMap m `addToMap` f }


-- | Private

addToMap :: (Foldable f, Ord a) => M.Map a b -> f (a, b) -> M.Map a b
addToMap = foldr (uncurry M.insert)

findInMap :: (Ord a) => M.Map a TimeValue -> a -> FinDate -> Maybe Double
findInMap m k d = ($ d) <$> M.lookup k m

