{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module ObservableExperiments where


-- | Experiments

-- TODO - Because it is so abstract, Observables cannot be saved in DB
-- => Only the product on top of it can be saved!
-- This module experiments with some ways to fix this.


data BinaryOp a b c where
    Add  :: (Num a) => BinaryOp a a a
    Mult :: (Num a) => BinaryOp a a a
    Less :: (Ord a) => BinaryOp a a Bool

class IBinaryOp f where
    type ArgT1 f
    type ArgT2 f
    type ResT  f
    marshall    :: f -> String
    unmarshall  :: String -> f
    apply       :: f -> ArgT1 f -> ArgT2 f -> ResT f

--data BinaryOpBox a b c  where
--    BinaryOpBox :: (IBinaryOp f) => f -> BinaryOpBox (ArgT1 f) (ArgT2 f) (ResT f)
--
--instance IBinaryOp (BinaryOpBox a b c) where
--    type ArgT1 (BinaryOpBox a b c) = a
--    type ArgT2 (BinaryOpBox a b c) = b
--    type ResT  (BinaryOpBox a b c) = c
--    marshall (BinaryOpBox f) = marshall f
--    unmarshall s = BinaryOpBox (unmarshall s) -- Cannot write it
--    apply (BinaryOpBox f) a b = apply f a b

data Addition a = Addition deriving (Show, Read)

instance (Num a) => IBinaryOp (Addition a) where
    type ArgT1 (Addition a) = a
    type ArgT2 (Addition a) = a
    type ResT  (Addition a) = a
    marshall = show
    unmarshall = read -- This is rather useless for unmarshalling - better make a correspondance in other side
    apply _ = (+)

data BinaryOp2 a b c = BinaryOp2 {
    apply2 :: a -> b -> c,
    fctId  :: Int
} -- Put that in a map int -> binaryOp2 (and add a representation)

data ObsValue a where
    BiFct :: (IBinaryOp f, ResT f ~ c) => { binaryOp' :: f,   arg1' :: ArgT1 f,  arg2' :: ArgT2 f  } -> ObsValue c

evalObsValue2 :: ObsValue a -> a
evalObsValue2 BiFct{..} = (apply binaryOp') arg1' arg2'


