module EvalProd (
    EvalEnv,
    newEnv,
    EvalProd,
    resultWithEnv,
    getStock,
    getRate,
    testEvalProd,
) where


import Control.Applicative
import Control.Arrow
import qualified Data.Map as M

import MarketData
import Utils.Monad
import Utils.Time



-- | Private:
-- | Result of the evaluation of a market data value

data Result a
    = Done a
    | Fail String

instance Functor Result where
    fmap f (Done a) = Done (f a)
    fmap f (Fail s) = Fail s

instance Applicative Result where
    pure         = Done
    Done f <*> a = fmap f a
    Fail s <*> _ = Fail s

instance Monad Result where
    return       = pure
    Done a >>= f = f a
    Fail s >>= _ = Fail s


-- | Abstract:
-- | Environment of evaluation of financial product

type Access m key res = key -> FinDate -> m (Result res)

data Cached m key res = Cached {
    access :: Access m key res,
    cache  :: M.Map (key, FinDate) (Result res)
}

data EvalEnv m = EvalEnv {
    stockAccess :: Cached m Stock Double,
    rateAccess  :: Cached m Rate  Double
}

newEnv :: Access m Stock Double -> Access m Rate Double -> EvalEnv m
newEnv s r = EvalEnv (toCached s) (toCached r)
    where
        toCached a = Cached { access = a, cache = M.empty }


-- | Abstract:
-- | Evaluation monad for the financial product

data EvalProd m a = EvalProd {
    runEvalProd :: EvalEnv m -> m (Result a, EvalEnv m)
}

resultWithEnv :: (Monad m) => EvalEnv m -> EvalProd m a -> m (Maybe a)
resultWithEnv env m = toMaybe . fst <$> runEvalProd m env
    where
        toMaybe (Done a) = Just a
        toMaybe _        = Nothing


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
                    Fail s -> pure (Fail s, env2)
                    Done a -> runEvalProd (f a) env


-- | Wrapped access to the monad logic

retrieve :: (Monad m, Ord key) => Cached m key res -> key -> FinDate -> m (Result res, Cached m key res)
retrieve cached key t =
    case M.lookup (key, t) (cache cached) of
        Just res -> pure (res, cached)
        Nothing -> do
            res <- access cached key t
            let newCache = M.insert (key, t) res (cache cached)
            let newCached = cached { cache = newCache }
            pure (res, newCached)

getStock :: (Monad m) => String -> FinDate -> EvalProd m Double
getStock s t = EvalProd $ \env -> do
    (res, newAccess) <- retrieve (stockAccess env) (Stock s) t
    pure (res, env { stockAccess = newAccess })

getRate :: (Monad m) => String -> FinDate -> EvalProd m Double
getRate s t = EvalProd $ \env -> do
    (res, newAccess) <- retrieve (rateAccess env) (Rate s) t
    pure (res, env { rateAccess = newAccess })


-- | Test function

testEvalProd :: IO ()
testEvalProd = do
    let env = newEnv
                (\_ _ -> return (Done 2))
                (\_ _ -> return (Done 1))

    t <- getCurrentTime
    let formula = sin (getStock "USD" t) + getStock "EUR" t * getRate "LIBOR" t
    res <- resultWithEnv env formula
    print res


