module Payoff.Product.FirstOf where

import Eval
import Observable
import Utils.Monad


-- | Algorithm to find the first element matching an observable predicate

findFirst :: (Monad m) => [ObsPredicate] -> [p] -> EvalProd m (Maybe p)
findFirst cs ps = do
    let conditions = fmap evalObs cs
    firstMatch <- findM fst (zip conditions ps)
    pure (fmap snd firstMatch)

