module EvalMonad (
    EvalMonad,
    Quantity,
    Predicate,
    withMarketData,
    evalVar,
) where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import MarketData
import TimeUtils


-- | Analog to a simple reader monad to evaluate the deals and read the indices

data EvalMonad a = EvalMonad { _reader :: Reader MarketData a }

runEvaluation :: EvalMonad a -> MarketData -> a
runEvaluation = runReader . _reader

withMarketData :: MarketData -> EvalMonad a -> a
withMarketData = flip runEvaluation

evalMonad :: (MarketData -> a) -> EvalMonad a
evalMonad f = EvalMonad . ReaderT $ Identity . f


instance Functor EvalMonad where
    fmap f a = evalMonad $ f . runEvaluation a

instance Applicative EvalMonad where
    pure a  = evalMonad $ const a
    f <*> a = evalMonad $ \m -> runEvaluation f m (runEvaluation a m)

instance Monad EvalMonad where
    return  = pure
    a >>= f = evalMonad $ \m -> runEvaluation (f (runEvaluation a m)) m


-- | Useful type aliases

type Predicate = EvalMonad Bool
type Quantity  = EvalMonad Double


-- | Evaluation the market value inside the monad

evalVar :: (Observable a b, Default b) => a -> FinDate -> EvalMonad b
evalVar s t = evalMonad $ \m -> fromMaybe def (observe m s t)

