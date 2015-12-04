module Eval.Class where

import Utils.Time


class IMonadEval evalMonad where
    getStock :: Monad m => String -> FinDate -> evalMonad m Double
    getRate  :: Monad m => String -> FinDate -> evalMonad m Double

