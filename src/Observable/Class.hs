{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Observable.Class where

import Data.Maybe
import Eval
import Observable.Dependency


-- | Class for the observables and the operations on them

class IObservable a b | a -> b where
    getDeps ::            a -> ObsDependencies
    fixing  :: Monad m => a -> EvalProd m a
    evalObs :: Monad m => a -> EvalProd m b

class IWrappable a b | a -> b where
    cst     :: b -> a
    unwrap  :: a -> Maybe b


-- | Utils for observable values

getAllDeps :: (IObservable a b) => [a] -> ObsDependencies
getAllDeps = mconcat . fmap getDeps


-- | Utils for wrappable values

isKnown :: (IWrappable a b) => a -> Bool
isKnown = isJust . unwrap

unsafeVal :: (IWrappable a b) => a -> b
unsafeVal = fromJust . unwrap

ifKnown :: (IWrappable a b) => a -> (b -> c) -> (a -> c) -> c
ifKnown obs f g = if isKnown obs then f (unsafeVal obs) else g obs

ifAllKnown :: (IWrappable a b) => [a] -> ([b] -> c) -> ([a] -> c) -> c
ifAllKnown allObs f g =
    if all isKnown allObs
        then f (map unsafeVal allObs)
        else g allObs

