{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Observable where

import Control.Applicative
import Data.Monoid
import EvalProd
import MarketData
import Utils.Time



-- | Data type to represent observable values

-- TODO - Because it is so abstract, it cannot be saved in DB => Only the product on top of it can be saved!

data Observable a where
    ObsConstant :: { obsConst :: a                                                              } -> Observable a
    ObsStock    :: { obsId    :: StockId,       obsTime :: FinDate                              } -> Observable Double
    ObsRate     :: { obsId    :: RateId,        obsTime :: FinDate                              } -> Observable Double
    ObsUnaryOp  :: { unaryOp  :: a -> b,        arg1    :: Observable a                         } -> Observable b
    ObsBinaryOp :: { binaryOp :: a -> b -> c,   arg1    :: Observable a,  arg2 :: Observable b  } -> Observable c

type Quantity   = Observable Double
type Predicate  = Observable Bool

cst :: a -> Observable a
cst = ObsConstant

liftOp :: (a -> b) -> Observable a -> Observable b
liftOp = ObsUnaryOp

liftOp2 :: (a -> b -> c) -> Observable a -> Observable b -> Observable c
liftOp2 = ObsBinaryOp


-- | To collect all the dependencies of an observable

data ObsDependencies = ObsDependencies {
    stockDeps :: [(StockId, FinDate)],
    rateDeps  :: [(RateId, FinDate)]
} deriving(Show)

instance Monoid ObsDependencies where
    mempty  = ObsDependencies [] []
    mappend a b =  ObsDependencies
        { stockDeps = stockDeps a <> stockDeps b
        , rateDeps  = rateDeps  a <> rateDeps  b }

evalDeps :: Observable a -> ObsDependencies
evalDeps ObsConstant{..} = mempty
evalDeps ObsStock{..}    = ObsDependencies { stockDeps = [(obsId, obsTime)], rateDeps = [] }
evalDeps ObsRate{..}     = ObsDependencies { stockDeps = [], rateDeps = [(obsId, obsTime)] }
evalDeps ObsUnaryOp{..}  = evalDeps arg1
evalDeps ObsBinaryOp{..} = evalDeps arg1 <> evalDeps arg2


-- | Evaluation the observables

evalObs :: (Monad m) => Observable a -> EvalProd m a
evalObs ObsConstant{..} = return obsConst
evalObs ObsStock{..}    = getStock obsId obsTime
evalObs ObsRate{..}     = getRate obsId obsTime
evalObs ObsUnaryOp{..}  = unaryOp <$> evalObs arg1
evalObs ObsBinaryOp{..} = binaryOp <$> evalObs arg1 <*> evalObs arg2


-- | Fixing of an observable (do not use too much alternative for performance consideration - no multiple evals)

fixing :: (Monad m) => Observable a -> EvalProd m (Observable a)
fixing c@ObsConstant{}   = return c
fixing o@ObsBinaryOp{..} = do
    lhs <- fixing arg1
    rhs <- fixing arg2
    pure $ case (lhs, rhs) of
        (ObsConstant lc, ObsConstant rc) -> cst (binaryOp lc rc)
        _                                -> liftOp2 binaryOp lhs rhs
fixing o@ObsUnaryOp{..} = do
    lhs <- fixing arg1
    pure $ case lhs of
        ObsConstant lc -> cst (unaryOp lc)
        _              -> liftOp unaryOp lhs
fixing o = fmap cst (evalObs o) <|> pure o


-- | Utils and nice instances to have

(.>.), (.<.), (.==.), (./=.), (.<=.), (.>=.) :: (Ord a) => Observable a -> Observable a -> Observable Bool
(.>.)   = liftOp2 (>)
(.<.)   = liftOp2 (<)
(.==.)  = liftOp2 (==)
(./=.)  = liftOp2 (/=)
(.<=.)  = liftOp2 (<=)
(.>=.)  = liftOp2 (>=)

(.&&.), (.||.) :: Observable Bool -> Observable Bool -> Observable Bool
(.&&.) = liftOp2 (&&)
(.||.) = liftOp2 (||)

instance Functor Observable where
    fmap = ObsUnaryOp

instance (Show a) => Show (Observable a) where
    show ObsConstant{..}    = "Constant { " ++ show obsConst ++ " }"
    show obsValue           = "Variable { stockDeps = " ++ show (stockDeps deps)
                                                        ++ ", rateDeps = "
                                                        ++ show (rateDeps deps) ++ " }"
        where deps = evalDeps obsValue

instance (Num a) => Num (Observable a) where
    (+) = liftOp2 (+)
    (*) = liftOp2 (*)
    (-) = liftOp2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = cst . fromInteger

instance (Fractional a) => Fractional (Observable a) where
    fromRational = cst . fromRational
    (/) = liftOp2 (/)

instance (Floating a) => Floating (Observable a) where
    pi  = cst pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh


