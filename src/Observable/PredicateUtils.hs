module Observable.PredicateUtils where

import Control.Monad
import Eval
import Observable.Class
import Observable.Predicate
import Observable.Types
import Utils.Monad


-- | Utils

(.==.), (./=.), (.>.), (.<.), (.<=.), (.>=.) :: ObsQuantity -> ObsQuantity -> ObsPredicate
(.==.) a b = QuantityRel IsEQ [a, b]
(./=.) a b = QuantityRel IsNEQ [a, b]
(.>.)  a b = QuantityRel IsGT [a, b]
(.<.)  a b = QuantityRel IsLT [a, b]
(.<=.) a b = QuantityRel IsLTE [a, b]
(.>=.) a b = QuantityRel IsGTE [a, b]

(.&&.), (.||.) :: ObsPredicate -> ObsPredicate -> ObsPredicate
(.&&.) a b = CombinePred And [a, b]
(.||.) a b = CombinePred Or  [a, b]

(.!.) :: ObsPredicate -> ObsPredicate
(.!.) a = CombinePred Nor [a]


-- | Algorithm to find the first element matching an observable predicate

findFirst :: (Monad m) => [ObsPredicate] -> [p] -> EvalProd m (Maybe p)
findFirst cs ps = do
    let conditions = fmap evalObs cs
    firstMatch <- findM fst (zip conditions ps)
    pure (fmap snd firstMatch)


-- Algorithm to find all elements matching their respective predicate

filterIf :: (Monad m) => [ObsPredicate] -> [p] -> EvalProd m [p]
filterIf cs ps = do
    let conditions = fmap evalObs cs
    matches <- filterM fst (zip conditions ps)
    pure (fmap snd matches)


