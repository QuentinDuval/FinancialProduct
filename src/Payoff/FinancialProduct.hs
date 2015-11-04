{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Payoff.FinancialProduct where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Monoid
import Eval
import Payoff.Flow
import Observable
import Utils.Monad
import Utils.Syntax
import Utils.Time


-- | Vocabulary to describe financial products

data CompRule                       -- | Composition rule
    = AllOfRule                     -- ^ All products are considered
    | FirstOfRule [ObsPredicate]    -- ^ First first matching predicate
    | BestOfRule Stock FinDate      -- ^ Best of a set of product (based on a reference stock)
    deriving (Show, Read, Eq, Ord)

data FinProduct
    = Tangible  FinDate Stock
    | Scale     ObsQuantity FinProduct
    | Compose   CompRule [FinProduct]
    | Empty
    deriving (Show, Read, Eq, Ord)

pattern AllOf ps        = Compose AllOfRule ps
pattern FirstOf cs ps   = Compose (FirstOfRule cs) ps
pattern BestOf s t ps   = Compose (BestOfRule s t) ps

instance Monoid FinProduct where
    mempty = Empty
    mappend x (AllOf xs) = AllOf (x:xs)
    mappend (AllOf xs) x = AllOf (x:xs)
    mappend a b = AllOf [a, b]
    mconcat = AllOf


-- | Combinators

stock, rate :: String -> FinDate -> ObsQuantity
stock = StockObs
rate  = RateObs

stockRate :: String -> String -> FinDate -> ObsQuantity    -- TODO: Try to have different kind of rates instead
stockRate s1 s2 t = stock s1 t / stock s2 t

trn :: Double -> FinDate -> String -> FinProduct
trn qty date instr = scale (cst qty) (Tangible date (Stock instr))

scale :: ObsQuantity -> FinProduct -> FinProduct
scale _ Empty           = Empty
scale q (Scale q' p)    = Scale (q * q') p                  -- TODO: optimize in case the observable is a constant
scale q p               = Scale q p

send, give :: FinProduct -> FinProduct
send = id
give = scale (cst (-1))

ifThen :: ObsPredicate -> FinProduct -> FinProduct
ifThen p a = eitherP p a Empty

eitherP :: ObsPredicate -> FinProduct -> FinProduct -> FinProduct
eitherP p a b = FirstOf [p, cst True] [a, b]

allOf :: [FinProduct] -> FinProduct
allOf = mconcat

bestOfBy :: Stock -> FinDate -> [FinProduct] -> FinProduct
bestOfBy = BestOf

instance IfThenElse ObsPredicate FinProduct where
    ifThenElse = eitherP


-- | Evaluation of the production of financial products

instance IObservable FinProduct [Flow] where

    getDeps Empty           = mempty
    getDeps Tangible{}      = mempty
    getDeps (Scale qty p)   = getDeps qty
    getDeps (FirstOf cs ps) = getAllDeps cs <> getAllDeps ps
    getDeps (Compose _ ps)  = getAllDeps ps

    fixing Empty            = pure Empty
    fixing t@Tangible{}     = pure t
    fixing (Scale qty p)    = Scale <$> fixing qty <*> fixing p
    fixing (AllOf ps)       = mconcat <$> mapM fixing ps
    fixing (BestOf s t ps)  = do
        products <- mapM fixing ps
        fmap fst (findBestProduct s t products) <|> pure (BestOf s t products)
    fixing (FirstOf cs ps)  = do
        conditions <- mapM fixing cs
        products <- mapM fixing ps
        findFirstProduct conditions products <|> pure (FirstOf conditions products)

    evalObs Empty           = return []
    evalObs (Tangible d i)  = return [Flow 1 d i]
    evalObs (AllOf ps)      = concatMapM evalObs ps
    evalObs (BestOf s t ps) = fmap snd (findBestProduct s t ps)
    evalObs (FirstOf cs ps) = findFirstProduct cs ps >>= evalObs
    evalObs (Scale qty p)   = do
        val <- evalObs qty
        flows <- evalObs p
        return $ overFlow (*val) <$> flows


-- | Utils

evalKnownFlows :: (Monad m) => FinProduct -> EvalProd m [Flow]
evalKnownFlows (AllOf ps)      = concat <$> mapM evalKnownFlows ps
evalKnownFlows (BestOf s t ps) = fmap snd (findBestProduct s t ps) <|> pure []
evalKnownFlows (FirstOf cs ps) = (findFirstProduct cs ps >>= evalKnownFlows) <|> pure []
evalKnownFlows p               = evalObs p <|> pure []


-- | Private

findFirstProduct :: (Monad m) => [ObsPredicate] -> [FinProduct] -> EvalProd m FinProduct
findFirstProduct cs ps = do
    let conditions = fmap evalObs cs
    firstMatch <- findM fst (zip conditions ps)
    return $ maybe Empty snd firstMatch

findBestProduct :: (Monad m) => Stock -> FinDate -> [FinProduct] -> EvalProd m (FinProduct, [Flow])
findBestProduct ref t ps = do
    evals <- forM ps $ \p -> do
        flows <- evalObs p
        converted <- mapM (compound t <=< convert ref) flows
        return ((p, flows), sum $ fmap flow converted)
    let best = maximumBy (compare `on` snd) evals
    return (fst best)

