module Eval.EvalProd (
    EvalEnv,
    newEnv,
    EvalProd,
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
-- TODO - Add some way to accumulate requests in the functor + applicative (and access should take list?)

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
-- | Evaluation monad for the financial product

data EvalProd m a = EvalProd {
    runEvalProd :: EvalEnv m -> m (Result a, EvalEnv m) }

resultWithEnv :: (Monad m) => EvalEnv m -> EvalProd m a -> m (Result a)
resultWithEnv env m = fst <$> runEvalProd m env


instance (Monad m) => Functor (EvalProd m) where
    fmap f m = EvalProd $ \env -> first (fmap f) <$> runEvalProd m env

instance (Monad m) => Applicative (EvalProd m) where
    pure a  = EvalProd $ \env -> pure (Done a, env)
    m <*> a = EvalProd $ \env1 -> do
                (f', env2) <- runEvalProd m env1
                case f' of
                    Fail s -> pure (Fail s, env2)
                    Done f -> runEvalProd (fmap f a) env2

instance (Monad m) => Monad (EvalProd m) where
    return  = pure
    (>>)    = (*>)
    m >>= f = EvalProd $ \env -> do
                (a', env2) <- runEvalProd m env
                case a' of
                    Done a -> runEvalProd (f a) env
                    Fail s -> pure (Fail s, env2)

instance (Monad m) => Alternative (EvalProd m) where
    empty   = EvalProd $ \env -> pure (Fail "empty", env)
    a <|> b = EvalProd $ \env -> do
                (a', env2) <- runEvalProd a env
                case a' of
                    Done a -> pure (Done a, env2)
                    Fail s -> runEvalProd b env2 -- Profit of the caching of the first


-- | Wrapped access to the monad logic

instance IMonadEval EvalProd where

    getStock s t = EvalProd $ \env -> do
        (res, newAccess) <- retrieve (stockAccess env) (Stock s) t
        pure (res, env { stockAccess = newAccess })

    getRate s t = EvalProd $ \env -> do
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


