{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Observable.Class where

import Data.Maybe
import Eval
import Observable.Dependency
import Utils.Time


-- | Class for the observables and the operations on them

class IFixable a where
    getDeps  ::                  a -> ObsDependencies
    fixing   :: IMarketEval m => a -> m a

class (IFixable a) => IObservable a b | a -> b where -- TODO: think about adding cst in it (but beware of type wrond deduction)
    evalObs  :: IMarketEval m => a -> m b
    unwrap   ::                  a -> Maybe b
    shiftObs ::                  a -> Shifter -> a


-- | Utils for observable values

getAllDeps :: (IFixable a) => [a] -> ObsDependencies
getAllDeps = mconcat . fmap getDeps


-- | Utils for wrappable values

isKnown :: (IObservable a b) => a -> Bool
isKnown = isJust . unwrap

unsafeVal :: (IObservable a b) => a -> b
unsafeVal = fromJust . unwrap

ifKnown :: (IObservable a b) => a -> (b -> c) -> (a -> c) -> c
ifKnown obs f g = if isKnown obs then f (unsafeVal obs) else g obs

ifAllKnown :: (IObservable a b) => [a] -> ([b] -> c) -> ([a] -> c) -> c
ifAllKnown allObs f g =
    if all isKnown allObs
        then f (map unsafeVal allObs)
        else g allObs

