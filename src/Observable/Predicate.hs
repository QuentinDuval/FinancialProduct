{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Observable.Predicate where

import Control.Applicative
import Data.Monoid
import Eval
import Observable.Class
import Observable.Quantity
import Utils.Foldable



-- | Predicates based on observations

data QuantityRel = IsLT | IsGT | IsEQ | IsLTE | IsGTE | IsNEQ
    deriving (Show, Read, Eq, Ord)

data PredicateOp = And | Or | Nor
    deriving (Show, Read, Eq, Ord)

data ObsPredicate
    = CstBool       { cstBool   :: Bool }
    | QuantityRel   { qtyRel    :: QuantityRel, targets :: [ObsQuantity] }
    | CombinePred   { predOp    :: PredicateOp, preds :: [ObsPredicate] }
    deriving (Show, Read, Eq, Ord)


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


-- | Implementations for observable

instance IWrappable ObsPredicate Bool where
    cst                     = CstBool
    unwrap CstBool{..}      = Just cstBool
    unwrap _                = Nothing

instance IObservable ObsPredicate Bool where

    getDeps CstBool{}       = mempty
    getDeps QuantityRel{..} = getAllDeps targets
    getDeps CombinePred{..} = getAllDeps preds

    fixing c@CstBool{}      = pure c
    fixing QuantityRel{..}  = do
        fixedQties <- mapM fixing targets
        pure $ ifAllKnown fixedQties (CstBool . applyQtyRel qtyRel) (QuantityRel qtyRel)
    fixing CombinePred{..}  = do
        fixedPreds <- mapM fixing preds
        pure $ ifAllKnown fixedPreds (CstBool . applyPredOp predOp) (CombinePred predOp)

    evalObs CstBool{..}     = pure cstBool
    evalObs QuantityRel{..} = applyQtyRel qtyRel <$> mapM evalObs targets
    evalObs CombinePred{..} = applyPredOp predOp <$> mapM evalObs preds

    shiftObs o@CstBool{} _         = o
    shiftObs o@QuantityRel{..} shifter = o { targets = fmap (`shiftObs` shifter) targets }
    shiftObs o@CombinePred{..} shifter = o { preds   = fmap (`shiftObs` shifter) preds }


-- | Private

applyQtyRel :: QuantityRel -> [Double] -> Bool
applyQtyRel IsLT = isMonotonic (<)
applyQtyRel IsGT = isMonotonic (>)
applyQtyRel IsEQ = isMonotonic (==)
applyQtyRel IsNEQ = isMonotonic (/=)
applyQtyRel IsLTE = isMonotonic (<=)
applyQtyRel IsGTE = isMonotonic (>=)

applyPredOp :: PredicateOp -> [Bool] -> Bool
applyPredOp And = and
applyPredOp Or  = or
applyPredOp Nor = not . or


