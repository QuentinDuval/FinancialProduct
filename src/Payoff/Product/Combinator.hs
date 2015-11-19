{-# LANGUAGE MultiParamTypeClasses #-}
module Payoff.Product.Combinator where

import Eval
import Payoff.Product.BestOf
import Payoff.Product.Core
import Observable
import Utils.Syntax
import Utils.Time



-- | Helpful combinators to build products

stock, rate :: String -> FinDate -> ObsQuantity
stock = StockObs
rate  = RateObs

stockRate :: String -> String -> FinDate -> ObsQuantity     -- TODO: Try to have different kind of rates instead
stockRate s1 s2 t = stock s1 t / stock s2 t

trn :: Double -> FinDate -> String -> FinProduct
trn qty date instr = scale (cst qty) (Tangible (Stock instr) date)

send, give :: FinProduct -> FinProduct
send = id
give = scale (cst (-1))

ifThen :: ObsPredicate -> FinProduct -> FinProduct
ifThen p a = eitherP p a Empty

eitherP :: ObsPredicate -> FinProduct -> FinProduct -> FinProduct
eitherP p a b = FirstOf [a, b] [p, cst True]

bestsOf :: Int -> [FinProduct] -> Stock -> FinDate -> FinProduct
bestsOf count products s t = BestOf products [BestOfParam count s t 0]

cascadingBestsOf :: [(Int, Shifter)] -> [FinProduct] -> Stock -> FinDate -> FinProduct
cascadingBestsOf shiftedEvals products s t =
    let toParam (count, shift) = BestOfParam count s t shift
    in BestOf products (fmap toParam shiftedEvals)

withEvalOn :: (Stock -> FinDate -> b) -> (Stock, FinDate) -> b
withEvalOn = uncurry


-- | Aliases

bestOf = bestsOf 1


-- | Helper instances

instance IfThenElse ObsPredicate FinProduct where
    ifThenElse = eitherP

instance Monoid FinProduct where
    mempty = Empty
    mappend a b = allOf [a, b]
    mconcat = allOf



