{-# LANGUAGE FlexibleInstances #-}
module Utils.Monad where

import Control.Applicative
import Control.Monad



-- | Helpful combinators to work on monadic values

(.>.), (.<.), (.==.), (./=.), (.<=.), (.>=.) :: (Applicative m) => m Double -> m Double -> m Bool
(.>.)   = liftA2 (>)
(.<.)   = liftA2 (<)
(.==.)  = liftA2 (==)
(./=.)  = liftA2 (/=)
(.<=.)  = liftA2 (<=)
(.>=.)  = liftA2 (>=)

(.&&.), (.||.) :: (Applicative m) => m Bool -> m Bool -> m Bool
(.&&.) = liftA2 (&&)
(.||.) = liftA2 (&&)


instance (Applicative m, Num a) => Num (m a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (Applicative m, Fractional a) => Fractional (m a) where
    fromRational = pure . fromRational
    (/) = liftA2 (/)

instance (Applicative m, Floating a) => Floating (m a) where
    pi  = pure pi
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


cst :: (Applicative m, Real a, Fractional b) => a -> m b
cst = pure . realToFrac


-- | Helpful algorithms to work on monadic values

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []      = return Nothing
findM p (x:xs)  = do
    v <- p x
    if v then return (Just x)
         else findM p xs


