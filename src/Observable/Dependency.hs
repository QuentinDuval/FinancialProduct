module Observable.Dependency where

import Data.Monoid
import MarketData
import Utils.Time



-- | Dependencies of the observations

data ObsDependencies = ObsDependencies {
    stockDeps :: [(StockId, FinDate)],
    rateDeps  :: [(RateId, FinDate)]
} deriving(Show)

instance Monoid ObsDependencies where
    mempty  = ObsDependencies [] []
    mappend a b =  ObsDependencies
        { stockDeps = stockDeps a <> stockDeps b
        , rateDeps  = rateDeps  a <> rateDeps  b }


