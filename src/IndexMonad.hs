-----------------------------------------------------------------------------
--
-- Module      :  IndexMonad
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IndexMonad where

import qualified Data.Map as M


-- | Very basic representation of a financial index

data FinIndex = FI String
    deriving (Show, Eq, Ord)

data Indexes = Indexes { indexMap :: M.Map FinIndex Double }

indexes :: [(FinIndex, Double)] -> Indexes
indexes = Indexes . M.fromList


-- | Analog to a simple reader monad to evaluate the deals and read the indices

data IndexMonad a = IndexMonad { runIndex :: Indexes -> a }

instance Functor IndexMonad where
    fmap f a = IndexMonad $ \m -> f (runIndex a m)

instance Applicative IndexMonad where
    pure a = IndexMonad $ const a
    f <*> a = IndexMonad $ \m -> runIndex f m (runIndex a m)

instance Monad IndexMonad where
    return = pure
    (IndexMonad a) >>= f = IndexMonad $ \m -> runIndex (f (a m)) m


-- | Useful type aliases

type Predicate = IndexMonad Bool
type Quantity  = IndexMonad Double


-- | Class of what it is possible to evaluate

evalIndex :: FinIndex -> Quantity
evalIndex i = IndexMonad $ \m -> M.findWithDefault 0 i (indexMap m)
