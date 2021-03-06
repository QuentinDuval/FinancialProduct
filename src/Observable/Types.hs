module Observable.Types where

import Eval
import Utils.Time


-- | Observable quantities

data QtyTransf = Neg | Abs | Sign | Inv
    deriving (Show, Read, Eq, Ord)

data QtyOp = Add | Mult
    deriving (Show, Read, Eq, Ord)

data ObsQuantity
    = CstQuantity    { cstQty :: Double }
    | StockObs       { obsId  :: StockId, obsTime :: FinDate }
    | RateObs        { obsId  :: RateId,  obsTime :: FinDate }
    | Transf         { transf :: QtyTransf, subQty :: ObsQuantity }
    | CombineQty     { qtyOp  :: QtyOp, quantities :: [ObsQuantity] }
--    | ConditionalQty { conds  :: [ObsPredicate], quantities :: [ObsQuantity] }
    deriving (Show, Read, Eq, Ord)


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
    -- Use the inversion of dependencies for the obs predicate that can apply on so many things
