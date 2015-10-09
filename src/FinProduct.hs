{-# LANGUAGE PatternSynonyms #-}
module FinProduct where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import EvalMonad
import Flow
import MarketData
import Utils.Monad
import Utils.Time


-- | Vocabulary to describe financial products

-- TODO: Add a BestOf product, but requires a "reference"
-- TODO: Remove date from tangible, and allow to set / shift date

data CompRule                   -- | Composition rule
    = AllOfRule                 -- ^ All products are considered
    | FirstOfRule [Predicate]   -- ^ First first matching predicate
    | BestOfRule Stock          -- ^ Best of a set of product (based on a reference stock)

data FinProduct
    = Tangible  FinDate Stock
    | Scale     Quantity FinProduct
    | Compose   CompRule [FinProduct]
    | Empty

pattern AllOf ps        = Compose AllOfRule ps
pattern FirstOf cs ps   = Compose (FirstOfRule cs) ps
pattern BestOf s ps     = Compose (BestOfRule s) ps

instance Monoid FinProduct where
    mempty = Empty
    mappend x (AllOf xs) = AllOf (x:xs)
    mappend (AllOf xs) x = AllOf (x:xs)
    mappend a b = AllOf [a, b]
    mconcat = AllOf


-- | Combinators

stock, rate :: String -> FinDate -> Quantity
stock = evalVar . Stock
rate  = evalVar . Rate

stockRate :: String -> String -> FinDate -> Quantity    -- TODO: Try to have different kind of rates instead
stockRate s1 s2 t = (/) <$> stock s1 t <*> stock s2 t

trn :: Double -> FinDate -> String -> FinProduct
trn qty date instr = scale (cst qty) (Tangible date (Stock instr))

scale :: Quantity -> FinProduct -> FinProduct
scale _ Empty           = Empty
scale q (Compose r ps)  = Compose r (scale q <$> ps)
scale q (Scale q' p)    = Scale (q * q') p
scale q p               = Scale q p

send, give :: FinProduct -> FinProduct
send = id
give = scale (cst (-1))

ifThen :: Predicate -> FinProduct -> FinProduct
ifThen p a = eitherP p a Empty

eitherP :: Predicate -> FinProduct -> FinProduct -> FinProduct
eitherP p a b = FirstOf [p, pure True] [a, b]

bestOfBy :: Stock -> [FinProduct] -> FinProduct
bestOfBy = BestOf


-- | Evaluation of the production of financial products

evalProduct :: FinProduct -> EvalMonad [Flow]
evalProduct Empty           = return []
evalProduct (Tangible d i)  = return [Flow 1 d i]
evalProduct (AllOf ps)      = concat <$> mapM evalProduct ps

evalProduct (FirstOf cs ps) = do
    firstMatch <- findM fst (zip cs ps)
    let p = maybe Empty snd firstMatch
    evalProduct p

evalProduct (BestOf ref ps) = do
    evals <- forM ps $ \p -> do
        flows <- evalProduct p
        converted <- mapM (convert ref) flows
        return (flows, sum $ fmap flow converted)
    let best = maximumBy (compare `on` snd) evals
    return (fst best)

evalProduct (Scale qty p)   = do
    val <- qty
    flows <- evalProduct p
    return $ overFlow (*val) <$> flows













