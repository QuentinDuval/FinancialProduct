{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Observable.Predicate (
    ObsPredicate(..),

) where

import Control.Applicative
import Data.Monoid
import Eval
import Observable.Class
import Observable.Quantity
import Observable.Types
import Utils.Foldable



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


