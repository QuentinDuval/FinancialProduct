{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE DeriveDataTypeable #-}
module ObservableExperiments where

import Data.Typeable
--import Data.Generics


-- | Experiments

-- TODO - Because it is so abstract, Observables cannot be saved in DB
-- => Only the product on top of it can be saved!
-- This module experiments with some ways to fix this.

-- TODO - Track: typeable make it possible to compare types
-- => It should be possible to fill a map with repr to type and invert


-- | Usign concrete types to represent the operations

data BinaryOp a b c where
    Add, Mult       :: (Num a)  => BinaryOp a a a
    Or, And         ::             BinaryOp Bool Bool Bool
    IsLess, IsMore  :: (Ord a)  => BinaryOp a a Bool
    IsEqual         :: (Eq a)   => BinaryOp a a Bool
    deriving (Typeable)

data UnaryOp a b where
    Neg, Abs, Sign  :: (Num a)          => UnaryOp a a
    Inv             :: (Fractional a)   => UnaryOp a a
    Not             ::                     UnaryOp Bool Bool

toBinaryFct :: BinaryOp a b c -> a -> b -> c
toBinaryFct Add     = (+)
toBinaryFct Mult    = (*)
toBinaryFct Or      = (||)
toBinaryFct And     = (&&)
toBinaryFct IsLess  = (<)
toBinaryFct IsMore  = (<)
toBinaryFct IsEqual = (==)

toUnaryFct :: UnaryOp a b -> a -> b
toUnaryFct Neg = negate
toUnaryFct Abs = abs
toUnaryFct Inv = recip
toUnaryFct Not = not

instance Show (BinaryOp a b c) where
    show Add  = "Add"
    show Mult = "Mult"
    show Or   = "Or"
    show And  = "And"
    show IsLess = "IsLess"
    show IsMore = "IsMore"
    show IsEqual = "IsEqual"

instance Show (UnaryOp a b) where
    show Neg = "Neg"
    show Abs = "Abs"
    show Inv = "Inv"
    show Not = "Not"


-- | Using type classes
-- Isn't it pretty similar? In the end, the type classes are more easy to extend

class IBinaryOp f where
    type ArgT1 f
    type ArgT2 f
    type ResT  f
    toString :: f -> String
    apply    :: f -> ArgT1 f -> ArgT2 f -> ResT f

data BinaryOpBox a b c  where
    BinaryOpBox :: (IBinaryOp f) => f -> BinaryOpBox (ArgT1 f) (ArgT2 f) (ResT f)

instance IBinaryOp (BinaryOpBox a b c) where
    type ArgT1 (BinaryOpBox a b c) = a
    type ArgT2 (BinaryOpBox a b c) = b
    type ResT  (BinaryOpBox a b c) = c
    toString (BinaryOpBox f) = toString f
    apply (BinaryOpBox f) a b = apply f a b

data Addition a = Addition
    deriving (Show, Typeable)

instance (Typeable a, Num a) => IBinaryOp (Addition a) where
    type ArgT1 (Addition a) = a
    type ArgT2 (Addition a) = a
    type ResT  (Addition a) = a
    toString t = show (typeOf t)
    apply _  = (+)


-- Another attempt?

data BinaryOp' a b c = BinaryOp' {
    apply'    :: a -> b -> c,
    toString' :: String
}

instance IBinaryOp (BinaryOp' a b c) where
    type ArgT1 (BinaryOp' a b c) = a
    type ArgT2 (BinaryOp' a b c) = b
    type ResT  (BinaryOp' a b c) = c
    toString (BinaryOp' _ s) = s
    apply (BinaryOp' f _) a b = f a b

instance Show (BinaryOp' a b c) where
    show = toString'

registerOp :: (Typeable a) => a -> (b -> c -> d) -> BinaryOp' b c d
registerOp t f = BinaryOp' { apply' = f, toString' = show (typeOf t) }


--data ObsValue a where
--    BiFct :: (IBinaryOp f, ResT f ~ c) => { binaryOp' :: f,   arg1' :: ArgT1 f,  arg2' :: ArgT2 f  } -> ObsValue c
--
--evalObsValue2 :: ObsValue a -> a
--evalObsValue2 BiFct{..} = (apply binaryOp') arg1' arg2'


