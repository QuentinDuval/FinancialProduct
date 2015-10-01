module MarketData where

import qualified Data.Map as M


-- | Market data set definition

data FinIndex = FI String
    deriving (Show, Eq, Ord)

data MarketData = MarketData {
      indexMap :: M.Map FinIndex Double
    } deriving (Show, Eq, Ord)


-- | Helpers to construct a market data set

indexes :: (Foldable f) => f (FinIndex, Double) -> MarketData
indexes = MarketData . foldr (uncurry M.insert) M.empty

