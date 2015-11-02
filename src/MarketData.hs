module MarketData where

import Utils.Time


-- | Market data definition

type StockId    = String
type RateId     = String

newtype Stock   = Stock { stockLabel :: StockId }
    deriving (Show, Read, Eq, Ord)

newtype Rate    = Rate  { rateLabel  :: RateId  }
    deriving (Show, Read, Eq, Ord)

