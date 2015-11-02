module Utils.Monad where

import Control.Applicative
import Control.Monad


-- | Helpful algorithms to work on monadic values

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []      = return Nothing
findM p (x:xs)  = do
    v <- p x
    if v then return (Just x)
         else findM p xs

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f


