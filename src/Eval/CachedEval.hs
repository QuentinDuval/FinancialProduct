module Eval.CachedEval (
    CachedEvalEnv,
    newCacheEnv,
    CachedEval,
    evalWithCache
) where

import Control.Applicative
import Control.Arrow
import qualified Data.Map as M
import Eval.Class
import Eval.MarketData
import Eval.Result
import Utils.Monad
import Utils.Time


-- | Abstract:
-- | Environment of evaluation with market data

-- TODO - Add a new env with dependencies resolving at the beginning
-- TODO - Might make evalWithoutCache and evalWithCache same function (typeclass)


data Cached m key res = Cached {
    access :: Access m key res,
    cache  :: M.Map (key, FinDate) (Result res)
}

data CachedEvalEnv m = CachedEvalEnv {
    stockAccess :: Cached m Stock Double,
    rateAccess  :: Cached m Rate  Double
}

newCacheEnv :: (MarketDataAccess m a) => a -> CachedEvalEnv m
newCacheEnv s = CachedEvalEnv (toCached $ stockValue s) (toCached $ rateValue s)
    where toCached a = Cached { access = a, cache = M.empty }


-- | Abstract:
-- | Evaluation monad with access to the market data

data CachedEval m a = CachedEval {
    runCachedEval :: CachedEvalEnv m -> m (Result a, CachedEvalEnv m)
}

evalWithCache :: (Monad m) => CachedEvalEnv m -> CachedEval m a -> m (Result a)
evalWithCache env m = fst <$> runCachedEval m env


-- | Instance of Monad

instance (Monad m) => Functor (CachedEval m) where
    fmap f m = CachedEval $ \env -> first (fmap f) <$> runCachedEval m env

instance (Monad m) => Applicative (CachedEval m) where
    pure a  = CachedEval $ \env -> pure (Done a, env)
    m <*> a = CachedEval $ \env1 -> do
                (f', env2) <- runCachedEval m env1
                case f' of
                    Fail s -> pure (Fail s, env2)
                    Done f -> runCachedEval (fmap f a) env2

instance (Monad m) => Monad (CachedEval m) where
    return  = pure
    (>>)    = (*>)
    m >>= f = CachedEval $ \env -> do
                (a', env2) <- runCachedEval m env
                case a' of
                    Fail s -> pure (Fail s, env2)
                    Done a -> runCachedEval (f a) env

instance (Monad m) => Alternative (CachedEval m) where
    empty   = CachedEval $ \env -> pure (Fail "empty", env)
    a <|> b = CachedEval $ \env -> do
                (a', env2) <- runCachedEval a env
                case a' of
                    Fail s -> runCachedEval b env2 -- Profit of the caching of the first
                    Done a -> pure (Done a, env2)


-- | Instance of IMarketEval

instance (Monad m) => IMarketEval (CachedEval m) where

    getStock s t = CachedEval $ \env -> do
        (res, newAccess) <- retrieve (stockAccess env) (Stock s) t
        pure (res, env { stockAccess = newAccess })

    getRate s t = CachedEval $ \env -> do
        (res, newAccess) <- retrieve (rateAccess env) (Rate s) t
        pure (res, env { rateAccess = newAccess })


-- | Private (cache access)

retrieve :: (Monad m, Ord key) => Cached m key res -> key -> FinDate -> m (Result res, Cached m key res)
retrieve cached key t =
    case M.lookup (key, t) (cache cached) of
        Just res -> pure (res, cached)
        Nothing -> do
            res <- access cached key t
            let newCache = M.insert (key, t) res (cache cached)
            let newCached = cached { cache = newCache }
            pure (res, newCached)



