{-# LANGUAGE FlexibleInstances #-}
module MonadUtils where

import Control.Applicative
import Control.Monad



-- | Helpful combinators to work on monadic values

(.>.), (.<.), (.==.), (./=.) :: (Monad m) => m Double -> m Double -> m Bool
(.>.)   = liftA2 (>)
(.<.)   = liftA2 (<)
(.==.)  = liftA2 (==)
(./=.)  = liftA2 (/=)


(.&&.), (.||.) :: (Monad m) => m Bool -> m Bool -> m Bool
(.&&.) = liftA2 (&&)
(.||.) = liftA2 (&&)


instance (Monad m, Num a) => Num (m a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger

cst :: (Applicative m, Real a, Fractional b) => a -> m b
cst = pure . realToFrac


-- | Helpful algorithms to work on monad values

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []      = return Nothing
findM p (x:xs)  = do
    v <- p x
    if v then return (Just x)
         else findM p xs


