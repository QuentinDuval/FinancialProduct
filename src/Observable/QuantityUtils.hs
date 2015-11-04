module Observable.QuantityUtils where

import Observable.Class
import Observable.Quantity
import Utils.Time


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

