module Eval.Result where



-- | Result of the evaluation of a market data value

data Result a
    = Done a
    | Fail String
    deriving (Show, Eq, Ord)

instance Functor Result where
    fmap f (Done a) = Done (f a)
    fmap f (Fail s) = Fail s

instance Applicative Result where
    pure         = Done
    Done f <*> a = fmap f a
    Fail s <*> _ = Fail s

instance Monad Result where
    return       = pure
    Done a >>= f = f a
    Fail s >>= _ = Fail s

