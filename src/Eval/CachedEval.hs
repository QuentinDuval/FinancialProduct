module Eval.CachedEval (
    EvalEnv,
    newEnv,
    CachedEval,
    resultWithEnv,
    getStock,
    getRate,
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
-- | Environment of evaluation of financial product

-- TODO - Add a new env with dependencies resolving at the beginning
-- This might be done from the outside if we provide a "bulk fetch"
-- then the client algorithm might choose this from the outside


data Cached m key res = Cached {
    access :: Access m key res,
    cache  :: M.Map (key, FinDate) (Result res)
}

data EvalEnv m = EvalEnv {
    stockAccess :: Cached m Stock Double,
    rateAccess  :: Cached m Rate  Double
}

newEnv :: (MarketDataAccess m a) => a -> EvalEnv m
newEnv s = EvalEnv (toCached $ stockValue s) (toCached $ rateValue s)
    where toCached a = Cached { access = a, cache = M.empty }


-- | Abstract:
-- | Evaluation monad with access to the market data

data CachedEval m a = CachedEval {
    runCachedEval :: EvalEnv m -> m (Result a, EvalEnv m)
}

resultWithEnv :: (Monad m) => EvalEnv m -> CachedEval m a -> m (Result a)
resultWithEnv env m = fst <$> runCachedEval m env

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
                    Done a -> runCachedEval (f a) env
                    Fail s -> pure (Fail s, env2)

instance (Monad m) => Alternative (CachedEval m) where
    empty   = CachedEval $ \env -> pure (Fail "empty", env)
    a <|> b = CachedEval $ \env -> do
                (a', env2) <- runCachedEval a env
                case a' of
                    Done a -> pure (Done a, env2)
                    Fail s -> runCachedEval b env2 -- Profit of the caching of the first


-- | Wrapped access to the monad logic

instance (Monad m) => IMarketEval (CachedEval m) where

    getStock s t = CachedEval $ \env -> do
        (res, newAccess) <- retrieve (stockAccess env) (Stock s) t
        pure (res, env { stockAccess = newAccess })

    getRate s t = CachedEval $ \env -> do
        (res, newAccess) <- retrieve (rateAccess env) (Rate s) t
        pure (res, env { rateAccess = newAccess })


-- | Private

retrieve :: (Monad m, Ord key) => Cached m key res -> key -> FinDate -> m (Result res, Cached m key res)
retrieve cached key t =
    case M.lookup (key, t) (cache cached) of
        Just res -> pure (res, cached)
        Nothing -> do
            res <- access cached key t
            let newCache = M.insert (key, t) res (cache cached)
            let newCached = cached { cache = newCache }
            pure (res, newCached)



