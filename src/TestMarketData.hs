{-# LANGUAGE MultiParamTypeClasses #-}
module TestMarketData where

import Control.Monad.Identity
import qualified Data.Map as M
import Data.Maybe
import Eval
import Utils.Time


-- | To perform some tests, a fake market data access point

type TimeValue  = FinDate -> Maybe Double

data TestMarketData = TestMarketData {
    stockMap          :: M.Map Stock TimeValue ,
    rateMap           :: M.Map Rate  TimeValue }

instance MarketDataAccess Identity TestMarketData where
    stockValue mds key t = pure $ findInMap (stockMap mds) key t
    rateValue  mds key t = pure $ findInMap (rateMap  mds) key t

testMdsAccess :: TestMarketData -> CachedEvalEnv Identity
testMdsAccess = newCacheEnv


-- | Helpers to construct a market data set

emptyMds :: TestMarketData
emptyMds = TestMarketData M.empty M.empty

initMds :: (Foldable f1, Foldable f2) => f1 (Stock, TimeValue) -> f2 (Rate, TimeValue) -> TestMarketData
initMds s r = addStocks s (addRates r emptyMds)

addStocks :: (Foldable f) => f (Stock, TimeValue) -> TestMarketData -> TestMarketData
addStocks f m = m { stockMap = stockMap m `addToMap` f }

addRates :: (Foldable f) => f (Rate, TimeValue) -> TestMarketData -> TestMarketData
addRates f m = m { rateMap = rateMap m `addToMap` f }

missingMarketData :: Result a
missingMarketData = Fail "No results found"


-- | Private

addToMap :: (Foldable f, Ord a) => M.Map a b -> f (a, b) -> M.Map a b
addToMap = foldr (uncurry M.insert)

findInMap :: (Ord a) => M.Map a TimeValue -> a -> FinDate -> Result Double
findInMap m k d = maybe missingMarketData Done (findInMap' m k d)

findInMap' :: (Ord a) => M.Map a TimeValue -> a -> FinDate -> Maybe Double
findInMap' m k d = maybe Nothing ($ d) (M.lookup k m)

