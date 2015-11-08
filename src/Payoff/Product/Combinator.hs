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
bestsOf n ps s t = BestOf ps (BestOfParams n s t)

bestsOfWith :: Int -> [(FinProduct, FinProduct)] -> Stock -> FinDate -> FinProduct
bestsOfWith n ps s t =
    let (proxies, products) = unzip ps
    in BestOfProxy products proxies (BestOfParams 1 s t)

withEvalOn :: (Stock -> FinDate -> b) -> (Stock, FinDate) -> b
withEvalOn = uncurry


-- | Aliases

bestOf     = bestsOf 1
bestOfWith = bestsOfWith 1


-- | Helper instances

instance IfThenElse ObsPredicate FinProduct where
    ifThenElse = eitherP

instance Monoid FinProduct where
    mempty = Empty
    mappend a b = allOf [a, b]
    mconcat = allOf



