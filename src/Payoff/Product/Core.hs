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
import Payoff.Flow
import Observable
import Utils.Monad
import Utils.Time


-- | Vocabulary to describe financial products

data FinProduct                                                                     -- TODO: Rename to "flow generator"?
    = Empty
    | Tangible      { tangible    :: Stock,      payDate :: FinDate }               -- TODO: Add a flow type?
    | Scale         { subProduct  :: FinProduct, scaling :: ObsQuantity }
    | AllOf         { subProducts :: [FinProduct] }                                 -- TODO: Add predicates as well?
    | FirstOf       { subProducts :: [FinProduct], predicates   :: [ObsPredicate] }
    | BestOf        { subProducts :: [FinProduct], bestOfParams :: BestOfParams }
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
    getDeps BestOf{..}      = getAllDeps subProducts <> getBestOfDeps bestOfParams subProducts
    getDeps composite       = getAllDeps (subProducts composite)

    fixing Empty            = pure Empty
    fixing t@Tangible{}     = pure t
    fixing Scale{..}        = Scale <$> fixing subProduct <*> fixing scaling
    fixing AllOf{..}        = AllOf <$> mapM fixing subProducts
    fixing FirstOf{..}      = findFirstProductFixing predicates subProducts
    fixing b@BestOf{..}     = do
        (fixedParams, fixed) <- fixingBestOf bestOfParams subProducts
        case fixedParams of
            [] -> fixing (allOf fixed)
            ps -> pure (BestOf fixed fixedParams)

    evalObs Empty           = pure []
    evalObs Tangible{..}    = pure [Flow 1 payDate tangible]
    evalObs AllOf{..}       = concatMapM evalObs subProducts
    evalObs BestOf{..}      = fmap snd (findBest bestOfParams subProducts)
    evalObs FirstOf{..}     = findFirstProduct predicates subProducts >>= evalObs
    evalObs Scale{..}       = do
        val <- evalObs scaling
        flows <- evalObs subProduct
        pure $ overFlow (*val) <$> flows

    shiftObs Empty          _       = Empty
    shiftObs p@Tangible{..} shifter = p { payDate = payDate `addDay` shifter }
    shiftObs Scale{..}      shifter = Scale { subProduct = shiftObs subProduct shifter
                                            , scaling = shiftObs scaling shifter }
    shiftObs AllOf{..}      shifter = AllOf $ fmap (`shiftObs` shifter) subProducts
    shiftObs BestOf{..}     shifter = BestOf { subProducts = fmap (`shiftObs` shifter) subProducts
                                             , bestOfParams = shiftRefDate bestOfParams shifter }
    shiftObs FirstOf{..}    shifter = FirstOf { subProducts = fmap (`shiftObs` shifter) subProducts
                                              , predicates = fmap (`shiftObs` shifter) predicates }


-- | Eval know flows only

evalKnownFlows :: (Monad m) => FinProduct -> EvalProd m [Flow]
evalKnownFlows AllOf{..}       = concatMapM evalKnownFlows subProducts
evalKnownFlows FirstOf{..}     = (findFirstProduct predicates subProducts >>= evalKnownFlows) <|> pure []
evalKnownFlows p               = evalObs p <|> pure []


-- | Private

findFirstProduct :: (Monad m) => [ObsPredicate] -> [FinProduct] -> EvalProd m FinProduct
findFirstProduct cs ps = fromMaybe Empty <$> findFirst cs ps

findFirstProductFixing :: (Monad m) => [ObsPredicate] -> [FinProduct] -> EvalProd m FinProduct
findFirstProductFixing cs ps = fromMaybe Empty <$> findFirstFixing cs ps (flip FirstOf)

findBest :: (Monad m) => BestOfParams -> [FinProduct] -> EvalProd m (FinProduct, [Flow])
findBest params products = first allOf <$> findBests params products



