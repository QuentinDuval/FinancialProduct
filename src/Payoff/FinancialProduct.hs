{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

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

data BestOfParams = BestOfParams { bestCount :: Int, refStock :: Stock, refDate :: FinDate }
    deriving (Show, Read, Eq, Ord)

data FinProduct
    = Empty
    | Tangible      { tangible    :: Stock,      payDate :: FinDate }               -- TODO add a flow type?
    | Scale         { subProduct  :: FinProduct, scaling :: ObsQuantity }
    | AllOf         { subProducts :: [FinProduct] }
    | FirstOf       { subProducts :: [FinProduct], predicates   :: [ObsPredicate] }
    | BestOf        { subProducts :: [FinProduct], bestOfParams :: BestOfParams }
    | BestOfProxy   { subProducts :: [FinProduct], proxyProducts :: [FinProduct], bestOfParams :: BestOfParams }
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

bestsOf :: Int -> [FinProduct] -> Stock -> FinDate -> FinProduct
bestsOf n ps s t = BestOf ps (BestOfParams n s t)

bestsOfWith :: Int -> [(FinProduct, FinProduct)] -> Stock -> FinDate -> FinProduct
bestsOfWith n ps s t =
    let (proxies, products) = unzip ps
    in BestOfProxy products proxies (BestOfParams 1 s t)

withEvalOn :: (Stock -> FinDate -> b) -> (Stock, FinDate) -> b
withEvalOn = uncurry

instance IfThenElse ObsPredicate FinProduct where
    ifThenElse = eitherP


-- | Aliases

bestOf     = bestsOf 1
bestOfWith = bestsOfWith 1


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
    pure $ maybe Empty snd firstMatch

findBest :: (Monad m) => BestOfParams -> [FinProduct] -> EvalProd m (FinProduct, [Flow])
findBest params products = first allOf <$> findBests evalObs params products

findBestWith :: (Monad m) => BestOfParams -> [FinProduct] -> [FinProduct] -> EvalProd m (FinProduct, [Flow])
findBestWith params products proxies =
    let bests = findBests (evalObs . fst) params (zip proxies products)
    in first (allOf . fmap snd) <$> bests

findBests :: (Monad m) => (p -> EvalProd m [Flow]) -> BestOfParams -> [p] -> EvalProd m ([p], [Flow])
findBests evalP BestOfParams{..} subProducts = do
    evals <- forM subProducts $ \p -> do
        flows <- evalP p
        converted <- mapM (compound refDate <=< convert refStock) flows
        pure ((p, flows), sum $ fmap flow converted)
    let bests = fst <$> sortBy (compare `on` snd) evals
    pure $ second concat (unzip bests)



