module Listed.Option.Core where

import Observable
import Payoff.Product
import Utils.Time


-- | Common data to most options

data OptionHeader = OptionHeader {
      premium       :: FinProduct
    , maturity      :: Shifter
} deriving (Show, Read, Eq, Ord)

data OptionBody = OptionBody {
      strike        :: Double
    , quantity      :: ObsQuantity
    , buyInstr      :: String
    , sellInstr     :: String
} deriving (Show, Read, Eq, Ord)


-- | Common patterns of options

data SimpleOption
    = SimpleOption OptionHeader OptionBody
    deriving (Show, Read, Eq, Ord)

data CompositeOption
    = CompositeOption OptionHeader [OptionBody]
    deriving (Show, Read, Eq, Ord)

