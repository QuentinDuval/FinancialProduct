module IndexMonad where

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as M
import MarketData



-- | Analog to a simple reader monad to evaluate the deals and read the indices

data IndexMonad a = IndexMonad { _reader :: Reader MarketData a }

runIndex :: IndexMonad a -> MarketData -> a
runIndex = runReader . _reader

indexMonad :: (MarketData -> a) -> IndexMonad a
indexMonad f = IndexMonad . ReaderT $ Identity . f


instance Functor IndexMonad where
    fmap f a = indexMonad $ f . runIndex a

instance Applicative IndexMonad where
    pure a  = indexMonad $ const a
    f <*> a = indexMonad $ \m -> runIndex f m (runIndex a m)

instance Monad IndexMonad where
    return  = pure
    a >>= f = indexMonad $ \m -> runIndex (f (runIndex a m)) m


-- | Useful type aliases

type Predicate = IndexMonad Bool
type Quantity  = IndexMonad Double


-- | Evaluation the value of an index inside the monad

evalIndex :: FinIndex -> Quantity
evalIndex i = indexMonad $ \m -> M.findWithDefault 0 i (indexMap m)

