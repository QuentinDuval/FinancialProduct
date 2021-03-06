{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Observable.Quantity (
    QtyTransf(..),
    QtyOp(..),
    ObsQuantity(..),
    applyQtyTransf,
    applyQtyOp,
) where

import Control.Applicative
import Data.Monoid
import Eval
import Observable.Class
import Observable.Dependency
import Observable.Types
import Utils.Foldable
import Utils.Time



-- | Implementations for observable

instance IFixable ObsQuantity where

    getDeps CstQuantity{}       = mempty
    getDeps StockObs{..}        = ObsDependencies { stockDeps = [(obsId, obsTime)], rateDeps = [] }
    getDeps RateObs{..}         = ObsDependencies { stockDeps = [], rateDeps = [(obsId, obsTime)] }
    getDeps Transf{..}          = getDeps subQty
    getDeps CombineQty{..}      = getAllDeps quantities

    fixing c@CstQuantity{}  = pure c
    fixing CombineQty{..}   = do
        fixedQties <- mapM fixing quantities
        pure $ ifAllKnown fixedQties (CstQuantity . foldl1 (applyQtyOp qtyOp)) (CombineQty qtyOp)
    fixing Transf{..}       = do
        fixedQty <- fixing subQty
        pure $ ifKnown fixedQty (CstQuantity . applyQtyTransf transf) (Transf transf)
    fixing o                = fmap CstQuantity (evalObs o) <|> pure o


instance IObservable ObsQuantity Double where

    unwrap CstQuantity{..}  = Just cstQty
    unwrap _                = Nothing

    evalObs CstQuantity{..} = pure cstQty
    evalObs StockObs{..}    = getStock obsId obsTime
    evalObs RateObs{..}     = getRate obsId obsTime
    evalObs Transf{..}      = applyQtyTransf transf <$> evalObs subQty
    evalObs CombineQty{..}  = foldl1 (applyQtyOp qtyOp) <$> mapM evalObs quantities

    shiftObs q@CstQuantity{} _         = q
    shiftObs q@StockObs{..}    shifter = q { obsTime    = obsTime `addDay` shifter }
    shiftObs q@RateObs{..}     shifter = q { obsTime    = obsTime `addDay` shifter }
    shiftObs q@Transf{..}      shifter = q { subQty     = subQty `shiftObs` shifter }
    shiftObs q@CombineQty{..}  shifter = q { quantities = fmap (`shiftObs` shifter) quantities }


-- | Operation effect

applyQtyTransf :: QtyTransf -> Double -> Double
applyQtyTransf Neg   = negate
applyQtyTransf Abs   = abs
applyQtyTransf Sign  = signum
applyQtyTransf Inv   = recip

applyQtyOp :: QtyOp -> Double -> Double -> Double
applyQtyOp Add  = (+)
applyQtyOp Mult = (*)




