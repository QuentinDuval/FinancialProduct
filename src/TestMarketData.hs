module TestMarketData where

import Control.Monad.Identity
import qualified Data.Map as M
import EvalProd
import MarketData
import Utils.Time


-- | To perform some tests, a fake market data access point

type TimeValue  = FinDate -> Double

data TestMarketData = TestMarketData
    { stockMap :: M.Map Stock TimeValue
    , rateMap  :: M.Map Rate  TimeValue }

testMdsAccess :: TestMarketData -> EvalEnv Identity
testMdsAccess mds = newEnv f g
    where
        f key t = return $ findInMap (stockMap mds) key t
        g key t = return $ findInMap (rateMap  mds) key t


-- | Helpers to construct a market data set

emptyMds :: TestMarketData
emptyMds = TestMarketData M.empty M.empty

initMds :: (Foldable f1, Foldable f2) => f1 (Stock, TimeValue) -> f2 (Rate, TimeValue) -> TestMarketData
initMds s r = addStocks s (addRates r emptyMds)

addStocks :: (Foldable f) => f (Stock, TimeValue) -> TestMarketData -> TestMarketData
addStocks f m = m { stockMap = stockMap m `addToMap` f }

addRates :: (Foldable f) => f (Rate, TimeValue) -> TestMarketData -> TestMarketData
addRates f m = m { rateMap = rateMap m `addToMap` f }


-- | Private

addToMap :: (Foldable f, Ord a) => M.Map a b -> f (a, b) -> M.Map a b
addToMap = foldr (uncurry M.insert)

findInMap :: (Ord a) => M.Map a TimeValue -> a -> FinDate -> Result Double
findInMap m k d = maybe (Fail "No results found") Done $ fmap ($ d) (M.lookup k m)

