module Observable.QuantityUtils where

import Observable.Class
import Observable.Quantity
import Utils.Time


-- | Constructors

stock, rate :: String -> FinDate -> ObsQuantity
stock = StockObs
rate  = RateObs

stockRate :: String -> String -> FinDate -> ObsQuantity     -- TODO: Try to have different kind of rates instead
stockRate s1 s2 t = stock s1 t / stock s2 t

liftQtyOp :: QtyOp -> ObsQuantity -> ObsQuantity -> ObsQuantity
liftQtyOp op a b = CombineQty op [a, b]


-- | Nice instances to help manipulating observable quantities

instance Num ObsQuantity where
    (+)     = liftQtyOp Add
    (*)     = liftQtyOp Mult
    negate  = Transf Neg
    abs     = Transf Abs
    signum  = Transf Sign
    fromInteger = cst . fromInteger

instance Fractional ObsQuantity where
    fromRational = cst . fromRational
    recip = Transf Inv


-- | The average value of a stock over the given dates

averageStock :: String -> [FinDate] -> ObsQuantity
averageStock s ts =
    let sumQuantities = CombineQty { qtyOp = Add, quantities = fmap (StockObs s) ts }
    in sumQuantities / fromIntegral (length ts)

averageStockRate :: String -> String -> [FinDate] -> ObsQuantity
averageStockRate s1 s2 ts =
    let obs1 = fmap (StockObs s1) ts
        obs2 = fmap (StockObs s2) ts
        rates = zipWith (/) obs1 obs2
    in CombineQty { qtyOp = Add, quantities = rates } / fromIntegral (length ts)

