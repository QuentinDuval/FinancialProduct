{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Observable.Quantity (
    QtyTransf(..),
    QtyOp(..),
    ObsQuantity(..),
    liftQtyOp
) where

import Control.Applicative
import Eval
import Observable.Class
import Observable.Dependency
import Utils.Time


-- | Observable quantities

data QtyTransf = Neg | Abs | Sign | Inv
    deriving (Show, Read, Eq, Ord)

data QtyOp = Add | Mult
    deriving (Show, Read, Eq, Ord)

data ObsQuantity
    = CstQuantity   { cstQty :: Double }
    | StockObs      { obsId  :: StockId, obsTime :: FinDate }
    | RateObs       { obsId  :: RateId,  obsTime :: FinDate }
    | Transf        { transf :: QtyTransf, subQty :: ObsQuantity }
    | CombineQty    { qtyOp  :: QtyOp, quantities  :: [ObsQuantity] }
    deriving (Show, Read, Eq, Ord)

liftQtyOp :: QtyOp -> ObsQuantity -> ObsQuantity -> ObsQuantity
liftQtyOp op a b = CombineQty op [a, b]


-- | Implementations for observable

instance IWrappable ObsQuantity Double where
    cst                     = CstQuantity
    unwrap CstQuantity{..}  = Just cstQty
    unwrap _                = Nothing

instance IObservable ObsQuantity Double where

    getDeps CstQuantity{}   = mempty
    getDeps StockObs{..}    = ObsDependencies { stockDeps = [(obsId, obsTime)], rateDeps = [] }
    getDeps RateObs{..}     = ObsDependencies { stockDeps = [], rateDeps = [(obsId, obsTime)] }
    getDeps Transf{..}      = getDeps subQty
    getDeps CombineQty{..}  = getAllDeps quantities

    fixing c@CstQuantity{}  = pure c
    fixing CombineQty{..}   = do
        fixedQties <- mapM fixing quantities
        pure $ ifAllKnown fixedQties (CstQuantity . foldl1 (applyQtyOp qtyOp)) (CombineQty qtyOp)
    fixing Transf{..}       = do
        fixedQty <- fixing subQty
        pure $ ifKnown fixedQty (CstQuantity . applyQtyTransf transf) (Transf transf)
    fixing o                = fmap CstQuantity (evalObs o) <|> pure o

    evalObs CstQuantity{..} = pure cstQty
    evalObs StockObs{..}    = getStock obsId obsTime
    evalObs RateObs{..}     = getRate obsId obsTime
    evalObs Transf{..}      = applyQtyTransf transf <$> evalObs subQty
    evalObs CombineQty{..}  = foldl1 (applyQtyOp qtyOp) <$> mapM evalObs quantities


-- | Private

applyQtyTransf :: QtyTransf -> Double -> Double
applyQtyTransf Neg   = negate
applyQtyTransf Abs   = abs
applyQtyTransf Sign  = signum
applyQtyTransf Inv   = recip

applyQtyOp :: QtyOp -> Double -> Double -> Double
applyQtyOp Add  = (+)
applyQtyOp Mult = (*)

