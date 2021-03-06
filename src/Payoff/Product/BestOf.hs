{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Payoff.Product.BestOf (
    BestOfParam(..),
    BestOfParams,
    shiftRefDate,
    getBestOfDeps,
    fixingBestOf,
    findBests,
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Data.Monoid
import Eval
import Observable
import Payoff.Flow
import Utils.Time


-- | Parameters for the best of behaviors

type BestOfParams = [BestOfParam]

data BestOfParam
    = BestOfParam { bestCount :: Int, refStock :: Stock, refDate :: FinDate, shift :: Shifter  }
    deriving (Show, Read, Eq, Ord)

shiftRefDate :: BestOfParams -> Shifter -> BestOfParams
shiftRefDate params shifter = fmap shiftParam params
    where
        shiftParam p@BestOfParam{..} = p { refDate = refDate `addDay` shifter }


-- | Algorithms

getBestOfDeps :: (IObservable p r) => BestOfParams -> [p] -> ObsDependencies
getBestOfDeps params products =
    let depsOfParam BestOfParam{..} = mempty { stockDeps = [(stockLabel refStock, refDate)] }
        depsByShift shift = getAllDeps $ fmap (`shiftObs` shift) products
    in mconcat [
        mconcat (fmap (depsByShift . shift) params),
        -- TODO list all tangibles and shift them as well!
        mconcat (fmap depsOfParam params) ]

fixingBestOf :: (IMarketEval m, IObservable p [Flow]) => BestOfParams -> [p] -> m (BestOfParams, [p])
fixingBestOf [] subProducts = pure ([], subProducts)
fixingBestOf bs subProducts =
    let fixed = fst <$> findBestWith (head bs) subProducts
        recur = fixingBestOf (tail bs) =<< fixed
    in recur <|> pure (bs, subProducts)

findBests :: (IMarketEval m, IObservable p [Flow]) => BestOfParams -> [p] -> m ([p], [Flow])
findBests [b] subProducts = findBestWith b subProducts -- TODO - how to handle the empty case?
findBests bs subProducts  = do
    (keptProducts, _) <- findBestWith (head bs) subProducts
    findBests (tail bs) keptProducts


-- | Private:

findBestWith :: (IMarketEval m, IObservable p [Flow]) => BestOfParam -> [p] -> m ([p], [Flow])
findBestWith params@BestOfParam{..} products =
    let proxies = fmap (`shiftObs` shift) products
        bests = findBestsImpl (evalObs . fst) params (zip proxies products)
    in first (fmap snd) <$> bests

findBestsImpl :: (IMarketEval m) => (p -> m [Flow]) -> BestOfParam -> [p] -> m ([p], [Flow])
findBestsImpl evalP BestOfParam{..} subProducts = do
    evals <- forM subProducts $ \p -> do
        flows <- evalP p
        converted <- mapM (compound refDate <=< convert refStock) flows
        pure ((p, flows), sum $ fmap flow converted)
    let bests = fst <$> sortBy (compare `on` snd) evals
    let kept = take bestCount (reverse bests)
    pure $ second concat (unzip kept)



