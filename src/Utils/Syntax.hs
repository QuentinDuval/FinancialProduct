{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
module Utils.Syntax where

import Prelude hiding (ifThenElse)


class IfThenElse a b where
    ifThenElse :: a -> b -> b -> b

instance IfThenElse Bool a where
    ifThenElse True  lhs _ = lhs
    ifThenElse False _ rhs = rhs

