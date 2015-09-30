{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FinProduct where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Lens

import Flow
import IndexMonad



-- | Vocabulary to describe financial products

data Product
    = Tangible  String              -- TODO: Add dates
    | Scale     Quantity Product
    | AllOf     [Product]
    | IfThen    Predicate Product

cst :: (Real a) => a -> Quantity
cst = return . realToFrac

var :: String -> Quantity
var = evalIndex . FI

trn :: Double -> String -> Product
trn qty instr = scale (pure qty) (Tangible instr)

scale :: Quantity -> Product -> Product
scale = Scale

choice :: Predicate -> Product -> Product -> Product
choice p a b = AllOf [IfThen p a, IfThen (not <$> p) b]



-- | Evaluation of the production of financial products

evalProduct :: Product -> IndexMonad [Flow]
evalProduct (Tangible s)  = return [Flow 1 s]
evalProduct (AllOf ps)    = concat <$> mapM evalProduct ps
evalProduct (IfThen p f)  = do
    v <- p
    if v then evalProduct f
         else return []
evalProduct (Scale q f) = do
    v <- q
    fs <- evalProduct f
    return $ over flow (*v) <$> fs













