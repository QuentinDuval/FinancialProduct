{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Payoff.Product.Core where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.Monoid
import Eval
import Payoff.Product.BestOf
import Payoff.Product.FirstOf
import Payoff.Flow
import Observable
import Utils.Monad
import Utils.Time


-- | Vocabulary to describe financial products

data FinProduct                                                                     -- TODO: Rename to "flow generator"?
    = Empty
    | Tangible      { tangible    :: Stock,      payDate :: FinDate }               -- TODO: Add a flow type?
    | Scale         { subProduct  :: FinProduct, scaling :: ObsQuantity }
    | AllOf         { subProducts :: [FinProduct] }
    | FirstOf       { subProducts :: [FinProduct], predicates   :: [ObsPredicate] }
    | BestOf        { subProducts :: [FinProduct], bestOfParams :: BestOfParams }
    | BestOfProxy   { subProducts :: [FinProduct], proxyProducts :: [FinProduct], bestOfParams :: BestOfParams }
    deriving (Show, Read, Eq, Ord)


-- | Smart constructors

allOf :: [FinProduct] -> FinProduct
allOf = combine . foldr addProd []
    where
        addProd Empty ps       = ps
        addProd (AllOf ps') ps = ps' ++ ps
        addProd p ps           = p : ps
        combine []             = Empty
        combine [x]            = x
        combine xs             = AllOf xs

scale :: ObsQuantity -> FinProduct -> FinProduct
scale _ Empty       = Empty
scale q s@Scale{..} = s { scaling = q * scaling }
scale q p           = Scale p q


-- | Evaluation of the production of financial products

instance IObservable FinProduct [Flow] where

    getDeps Empty           = mempty
    getDeps Tangible{}      = mempty
    getDeps Scale{..}       = getDeps subProduct <> getDeps scaling
    getDeps FirstOf{..}     = getAllDeps predicates <> getAllDeps subProducts
    getDeps BestOfProxy{..} = getAllDeps proxyProducts <> getAllDeps subProducts
    getDeps composite       = getAllDeps (subProducts composite)

    fixing Empty            = pure Empty
    fixing t@Tangible{}     = pure t
    fixing Scale{..}        = Scale <$> fixing subProduct <*> fixing scaling
    fixing AllOf{..}        = AllOf <$> mapM fixing subProducts
    fixing b@BestOf{..}     = do
        products <- mapM fixing subProducts
        let fixed = b { subProducts = products }
        fmap fst (findBest bestOfParams products) <|> pure fixed
    fixing b@BestOfProxy{..}= do
        products <- mapM fixing subProducts
        proxies <- mapM fixing proxyProducts
        let fixed = b { subProducts = products, proxyProducts = proxies }
        fmap fst (findBestWith bestOfParams products proxies) <|> pure fixed
    fixing f@FirstOf{..}    = do
        conditions <- mapM fixing predicates
        products <- mapM fixing subProducts
        let fixed = FirstOf products conditions
        findFirstProduct conditions products <|> pure fixed

    evalObs Empty           = pure []
    evalObs Tangible{..}    = pure [Flow 1 payDate tangible]
    evalObs AllOf{..}       = concatMapM evalObs subProducts
    evalObs BestOf{..}      = fmap snd (findBest bestOfParams subProducts)
    evalObs FirstOf{..}     = findFirstProduct predicates subProducts >>= evalObs
    evalObs BestOfProxy{..} = fmap snd (findBestWith bestOfParams subProducts proxyProducts)
    evalObs Scale{..}       = do
        val <- evalObs scaling
        flows <- evalObs subProduct
        pure $ overFlow (*val) <$> flows


-- | Eval know flows only

evalKnownFlows :: (Monad m) => FinProduct -> EvalProd m [Flow]
evalKnownFlows AllOf{..}       = concatMapM evalKnownFlows subProducts
evalKnownFlows FirstOf{..}     = (findFirstProduct predicates subProducts >>= evalKnownFlows) <|> pure []
evalKnownFlows p               = evalObs p <|> pure []


-- | Private

findFirstProduct :: (Monad m) => [ObsPredicate] -> [FinProduct] -> EvalProd m FinProduct
findFirstProduct cs ps = fromMaybe Empty <$> findFirst cs ps

findBest :: (Monad m) => BestOfParams -> [FinProduct] -> EvalProd m (FinProduct, [Flow])
findBest params products = first allOf <$> findBests evalObs params products

findBestWith :: (Monad m) => BestOfParams -> [FinProduct] -> [FinProduct] -> EvalProd m (FinProduct, [Flow])
findBestWith params products proxies =
    let bests = findBests (evalObs . fst) params (zip proxies products)
    in first (allOf . fmap snd) <$> bests


