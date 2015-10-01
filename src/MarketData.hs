module MarketData where

import qualified Data.Map as M
import TimeUtils


-- | Market data set definition

data FinIndex = FI String
    deriving (Show, Eq, Ord)

data MarketData = MarketData { indexMap :: M.Map FinIndex (FinDate -> Double) }


-- | Helpers to construct a market data set

indexes :: (Foldable f) => f (FinIndex, FinDate -> Double) -> MarketData
indexes = MarketData . foldr (uncurry M.insert) M.empty

