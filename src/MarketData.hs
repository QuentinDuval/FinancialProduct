module MarketData where

import qualified Data.Map as M
import TimeUtils


-- | Market data set definition

data FinVar
    = Stock String
    | Rate  String
    deriving (Show, Eq, Ord)

data MarketData = MarketData { indexMap :: M.Map FinVar (FinDate -> Double) }


-- | Helpers to construct a market data set

indexes :: (Foldable f) => f (FinVar, FinDate -> Double) -> MarketData
indexes = MarketData . foldr (uncurry M.insert) M.empty

