module EvalProd (
    testEvalProd,
) where


import Control.Applicative
import Control.Arrow
import qualified Data.Map as M

import MarketData
import Utils.Monad
import Utils.Time


type StockCache = M.Map (Stock, FinDate) (Result Double)

data EvalEnv m = EvalEnv {
    stockAccess :: Stock -> FinDate -> m (Result Double),
    stockCache  :: StockCache
}

data Result a
    = Done a
    | Fail String
    deriving (Show)

instance Functor Result where
    fmap f (Done a) = Done (f a)
    fmap f (Fail s) = Fail s


data EvalProd m a = EvalProd {
    runEvalProd :: EvalEnv m -> m (Result a, EvalEnv m)
}

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
                    Fail s -> pure (Fail s, env2)
                    Done a -> runEvalProd (f a) env


-- | Read some information from the environment

getStock :: (Monad m) => Stock -> FinDate -> EvalProd m Double
getStock s t = EvalProd $ \env -> do
    let cacheState = stockCache env
    case M.lookup (s, t) cacheState of
        Just res -> pure (res, env)
        Nothing -> do
            res <- stockAccess env s t
            let newCache = M.insert (s, t) res cacheState
            let newEnv = env { stockCache = newCache }
            pure (res, newEnv)


-- | Test function

testEvalProd :: IO ()
testEvalProd = do
    t <- getCurrentTime
    let env = EvalEnv {
                stockAccess = \_ _ -> return (Done 2),
                stockCache  = M.empty }

    res <- resultWithEnv env $ sin $ getStock (Stock "USD") t + getStock (Stock "EUR") t
    print res
    return ()



