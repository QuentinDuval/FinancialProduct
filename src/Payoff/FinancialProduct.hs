{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Payoff.FinancialProduct where

import Control.Applicative
import Control.Arrow
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

data FinProduct
    = Empty
    | Tangible  { tangible    :: Stock,      payDate :: FinDate }
    | Scale     { subProduct  :: FinProduct, scaling :: ObsQuantity }
    | AllOf     { subProducts :: [FinProduct] }
    | FirstOf   { subProducts :: [FinProduct], predicates :: [ObsPredicate] }
    | BestOf    { subProducts :: [FinProduct], bestCount :: Int,
                  refStock :: Stock, refDate :: FinDate }           -- TODO add proxy product for "best" (but should not eval twice)
    deriving (Show, Read, Eq, Ord)

instance Monoid FinProduct where
    mempty = Empty
    mappend a b = allOf [a, b]
    mconcat = allOf


-- | Combinators

stock, rate :: String -> FinDate -> ObsQuantity
stock = StockObs
rate  = RateObs

stockRate :: String -> String -> FinDate -> ObsQuantity    -- TODO: Try to have different kind of rates instead
stockRate s1 s2 t = stock s1 t / stock s2 t

trn :: Double -> FinDate -> String -> FinProduct
trn qty date instr = scale (cst qty) (Tangible (Stock instr) date)

scale :: ObsQuantity -> FinProduct -> FinProduct
scale _ Empty       = Empty
scale q s@Scale{..} = s { scaling = q * scaling }           -- TODO: optimize in case the observable is a constant
scale q p           = Scale p q

send, give :: FinProduct -> FinProduct
send = id
give = scale (cst (-1))

ifThen :: ObsPredicate -> FinProduct -> FinProduct
ifThen p a = eitherP p a Empty

eitherP :: ObsPredicate -> FinProduct -> FinProduct -> FinProduct
eitherP p a b = FirstOf [a, b] [p, cst True]

allOf :: [FinProduct] -> FinProduct
allOf = combine . foldr addProd []
    where
        addProd Empty ps       = ps
        addProd (AllOf ps') ps = ps' ++ ps
        addProd p ps           = p : ps
        combine []             = Empty
        combine [x]            = x
        combine xs             = AllOf xs

bestOfBy :: Stock -> FinDate -> [FinProduct] -> FinProduct
bestOfBy s t ps = BestOf ps 1 s t

instance IfThenElse ObsPredicate FinProduct where
    ifThenElse = eitherP


-- | Evaluation of the production of financial products

instance IObservable FinProduct [Flow] where

    getDeps Empty           = mempty
    getDeps Tangible{}      = mempty
    getDeps Scale{..}       = getDeps subProduct <> getDeps scaling
    getDeps FirstOf{..}     = getAllDeps predicates <> getAllDeps subProducts
    getDeps composite       = getAllDeps (subProducts composite)

    fixing Empty            = pure Empty
    fixing t@Tangible{}     = pure t
    fixing Scale{..}        = Scale <$> fixing subProduct <*> fixing scaling
    fixing AllOf{..}        = AllOf <$> mapM fixing subProducts
    fixing b@BestOf{..}     = do
        products <- mapM fixing subProducts
        let fixed = b { subProducts = products }
        fmap fst (findBestProducts fixed) <|> pure fixed
    fixing f@FirstOf{..}    = do
        conditions <- mapM fixing predicates
        products <- mapM fixing subProducts
        let fixed = FirstOf products conditions
        findFirstProduct conditions products <|> pure fixed

    evalObs Empty           = return []
    evalObs Tangible{..}    = return [Flow 1 payDate tangible]
    evalObs AllOf{..}       = concatMapM evalObs subProducts
    evalObs b@BestOf{}      = fmap snd (findBestProducts b)
    evalObs FirstOf{..}     = findFirstProduct predicates subProducts >>= evalObs
    evalObs Scale{..}       = do
        val <- evalObs scaling
        flows <- evalObs subProduct
        return $ overFlow (*val) <$> flows


-- | Utils

evalKnownFlows :: (Monad m) => FinProduct -> EvalProd m [Flow]
evalKnownFlows AllOf{..}       = concatMapM evalKnownFlows subProducts
evalKnownFlows FirstOf{..}     = (findFirstProduct predicates subProducts >>= evalKnownFlows) <|> pure []
evalKnownFlows p               = evalObs p <|> pure []


-- | Private

findFirstProduct :: (Monad m) => [ObsPredicate] -> [FinProduct] -> EvalProd m FinProduct
findFirstProduct cs ps = do
    let conditions = fmap evalObs cs
    firstMatch <- findM fst (zip conditions ps)
    return $ maybe Empty snd firstMatch

findBestProducts :: (Monad m) => FinProduct -> EvalProd m (FinProduct, [Flow])
findBestProducts BestOf{..} = do
    evals <- forM subProducts $ \p -> do
        flows <- evalObs p
        converted <- mapM (compound refDate <=< convert refStock) flows
        return ((p, flows), sum $ fmap flow converted)
    let bests = fst <$> sortBy (compare `on` snd) evals
    pure $ (allOf *** concat) (unzip bests)

