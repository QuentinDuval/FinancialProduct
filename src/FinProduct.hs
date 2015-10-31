{-# LANGUAGE PatternSynonyms #-}
module FinProduct where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import EvalProd
import Flow
import MarketData
import Observable
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
stock = ObsStock
rate  = ObsRate

stockRate :: String -> String -> FinDate -> Quantity    -- TODO: Try to have different kind of rates instead
stockRate s1 s2 t = stock s1 t / stock s2 t

trn :: Double -> FinDate -> String -> FinProduct
trn qty date instr = scale (cst qty) (Tangible date (Stock instr))

scale :: Quantity -> FinProduct -> FinProduct
scale _ Empty           = Empty
scale q (Scale q' p)    = Scale (q * q') p    -- TODO: optimize in case the observable is a constant
scale q p               = Scale q p

send, give :: FinProduct -> FinProduct
send = id
give = scale (cst (-1))

ifThen :: Predicate -> FinProduct -> FinProduct
ifThen p a = eitherP p a Empty

eitherP :: Predicate -> FinProduct -> FinProduct -> FinProduct
eitherP p a b = FirstOf [p, cst True] [a, b]

bestOfBy :: Stock -> [FinProduct] -> FinProduct
bestOfBy = BestOf


-- | Evaluation of the production of financial products

evalProduct :: (Monad m) => FinProduct -> EvalProd m [Flow]
evalProduct Empty           = return []
evalProduct (Tangible d i)  = return [Flow 1 d i]
evalProduct (AllOf ps)      = concat <$> mapM evalProduct ps
evalProduct (BestOf ref ps) = fmap snd (findBestProduct ref ps)
evalProduct (FirstOf cs ps) = findFirstProduct cs ps >>= evalProduct
evalProduct (Scale qty p)   = do
    val <- evalObs qty
    flows <- evalProduct p
    return $ overFlow (*val) <$> flows

evalKnownFlows :: (Monad m) => FinProduct -> EvalProd m [Flow]
evalKnownFlows (AllOf ps)      = concat <$> mapM evalKnownFlows ps
evalKnownFlows (BestOf ref ps) = fmap snd (findBestProduct ref ps) <|> pure []
evalKnownFlows (FirstOf cs ps) = (findFirstProduct cs ps >>= evalKnownFlows) <|> pure []
evalKnownFlows p               = evalProduct p <|> pure []


-- | Fixing of the production of financial products

fixProduct :: (Monad m) => FinProduct -> EvalProd m FinProduct
fixProduct (Scale qty p)    = Scale <$> fixing qty <*> fixProduct p
fixProduct (AllOf ps)       = mconcat <$> mapM fixProduct ps
fixProduct (BestOf ref ps)  = do
    products <- mapM fixProduct ps
    fmap fst (findBestProduct ref products) <|> pure (BestOf ref products)
fixProduct (FirstOf cs ps)  = do
    conditions <- mapM fixing cs
    products <- mapM fixProduct ps
    findFirstProduct conditions products <|> pure (FirstOf conditions products)
fixProduct p                = pure p


-- | Private

findFirstProduct :: (Monad m) => [Predicate] -> [FinProduct] -> EvalProd m FinProduct
findFirstProduct cs ps = do
    let conditions = fmap evalObs cs
    firstMatch <- findM fst (zip conditions ps)
    return $ maybe Empty snd firstMatch

findBestProduct :: (Monad m) => Stock -> [FinProduct] -> EvalProd m (FinProduct, [Flow])
findBestProduct ref ps = do
    evals <- forM ps $ \p -> do
        flows <- evalProduct p
        converted <- mapM (convert ref) flows
        return ((p, flows), sum $ fmap flow converted)
    let best = maximumBy (compare `on` snd) evals
    return (fst best)


-- | Nice instance to have

instance Show FinProduct where
    show (Tangible d s)  = "Tangible { date = " ++ show d ++ ", stock = " ++ show (stockLabel s) ++ " }"
    show (Scale q p)     = "Scale [ " ++ show q ++ " ] { " ++ show p ++ " }"
    show (AllOf ps)      = "AllOf { " ++ show ps ++ " }"
    show (FirstOf cs ps) = "FirstOf { " ++ show cs ++ ", " ++ show ps ++ " }"
    show (BestOf s ps)   = "BestOf { ref = " ++ show (stockLabel s) ++ ", " ++ show ps ++ " }"
    show _               = ""










