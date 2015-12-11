module Eval.NoCacheEval (
    NoCacheEvalEnv,
    newNoCacheEnv,
    NoCacheEval,
    evalWithoutCache
) where

import Control.Applicative
import Eval.Class
import Eval.MarketData
import Eval.Result
import Utils.Monad
import Utils.Time


-- | Abstract:
-- | Environment of evaluation with market data

data NoCacheEvalEnv m = NoCacheEvalEnv {
    stockAccess :: Access m Stock Double,
    rateAccess  :: Access m Rate  Double
}

newNoCacheEnv :: (MarketDataAccess m a) => a -> NoCacheEvalEnv m
newNoCacheEnv s = NoCacheEvalEnv (stockValue s) (rateValue s)


-- | Abstract:
-- | Evaluation monad with access to the market data

data NoCacheEval m a = NoCacheEval {
    runNoCacheEval :: NoCacheEvalEnv m -> m (Result a)
}

evalWithoutCache :: (Monad m) => NoCacheEvalEnv m -> NoCacheEval m a -> m (Result a)
evalWithoutCache = flip runNoCacheEval


-- | Instance of Monad (just a reader)

instance (Monad m) => Functor (NoCacheEval m) where
    fmap f m = NoCacheEval $ \env -> fmap f <$> runNoCacheEval m env

instance (Monad m) => Applicative (NoCacheEval m) where
    pure a  = NoCacheEval $ \env -> pure (Done a)
    m <*> a = NoCacheEval $ \env -> do
                f' <- runNoCacheEval m env
                case f' of
                    Fail s -> pure (Fail s)
                    Done f -> runNoCacheEval (fmap f a) env

instance (Monad m) => Monad (NoCacheEval m) where
    return  = pure
    m >>= f = NoCacheEval $ \env -> do
                a' <- runNoCacheEval m env
                case a' of
                    Fail s -> pure (Fail s)
                    Done a -> runNoCacheEval (f a) env

instance (Monad m) => Alternative (NoCacheEval m) where
    empty   = NoCacheEval $ \env -> pure (Fail "empty")
    a <|> b = NoCacheEval $ \env -> do
                a' <- runNoCacheEval a env
                case a' of
                    Fail s -> runNoCacheEval b env
                    Done a -> pure (Done a)


-- | Instance of IMarketEval

instance (Monad m) => IMarketEval (NoCacheEval m) where
    getStock s t = NoCacheEval $ \env -> stockAccess env (Stock s) t
    getRate  s t = NoCacheEval $ \env -> rateAccess env (Rate s) t


