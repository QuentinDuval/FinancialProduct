module EvalMonad where

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as M
import MarketData



-- | Analog to a simple reader monad to evaluate the deals and read the indices

data EvalMonad a = EvalMonad { _reader :: Reader MarketData a }

runIndex :: EvalMonad a -> MarketData -> a
runIndex = runReader . _reader

evalMonad :: (MarketData -> a) -> EvalMonad a
evalMonad f = EvalMonad . ReaderT $ Identity . f


instance Functor EvalMonad where
    fmap f a = evalMonad $ f . runIndex a

instance Applicative EvalMonad where
    pure a  = evalMonad $ const a
    f <*> a = evalMonad $ \m -> runIndex f m (runIndex a m)

instance Monad EvalMonad where
    return  = pure
    a >>= f = evalMonad $ \m -> runIndex (f (runIndex a m)) m


-- | Useful type aliases

type Predicate = EvalMonad Bool
type Quantity  = EvalMonad Double


-- | Evaluation the value of an index inside the monad

evalIndex :: FinIndex -> Quantity
evalIndex i = evalMonad $ \m -> M.findWithDefault 0 i (indexMap m)

