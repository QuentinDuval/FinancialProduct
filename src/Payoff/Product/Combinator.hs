{-# LANGUAGE MultiParamTypeClasses #-}
module Payoff.Product.Combinator where

import Eval
import Payoff.Product.BestOf
import Payoff.Product.Core
import Observable
import Utils.Syntax
import Utils.Time



-- | Helpful combinators to build products

recv, send :: Double -> FinDate -> String -> FinProduct
recv qty date instr = scale (cst qty) (Tangible (Stock instr) date)
send qty = recv (-qty)

give :: FinProduct -> FinProduct
give = scale (cst (-1))

ifThen :: ObsPredicate -> FinProduct -> FinProduct
ifThen p a = eitherP p a Empty

eitherP :: ObsPredicate -> FinProduct -> FinProduct -> FinProduct
eitherP p a b = FirstOf [a, b] [p, CstBool True]

bestsOf :: Int -> [FinProduct] -> Stock -> FinDate -> FinProduct
bestsOf count products = cascadingBestsOf products [(1, 0)]

cascadingBestsOf :: [FinProduct] -> [(Int, Shifter)] -> Stock -> FinDate -> FinProduct
cascadingBestsOf products shiftedEvals s t =
    let toParam (count, shift) = BestOfParam count s t shift
    in BestOf products (fmap toParam shiftedEvals)

withEvalOn :: (Stock -> FinDate -> b) -> (Stock, FinDate) -> b
withEvalOn = uncurry

plus :: FinProduct -> FinProduct -> FinProduct
plus a b = allOf [a, b]


-- | Aliases

bestOf = bestsOf 1


-- | Helper instances

instance IfThenElse ObsPredicate FinProduct where
    ifThenElse = eitherP

instance Monoid FinProduct where
    mempty = Empty
    mappend a b = allOf [a, b]
    mconcat = allOf



