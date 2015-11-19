{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Payoff.Product.BestOf where

import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Eval
import Observable
import Payoff.Flow
import Utils.Time


-- | Parameters for the best of behaviors

-- TODO - Check shifts are ordered (at construction?)
-- TODO - The problem with this model is that dependencies are not working
-- TODO - The problem with this model is that fixing is not working - but we can make it work (remove head by head - limited form)

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
getBestOfDeps = undefined -- TODO - Apply the shifters to deduce dependencies

fixingBestOf :: (Monad m, IObservable p [Flow]) => BestOfParams -> [p] -> EvalProd m (BestOfParams, [p])
fixingBestOf = undefined -- TODO - Try to fix each stage one by one

findBests :: (Monad m, IObservable p [Flow]) => BestOfParams -> [p] -> EvalProd m ([p], [Flow])
findBests [b] subProducts = findBestWith b subProducts -- TODO - how to handle the empty case?
findBests bs subProducts  = do
    (keptProducts, _) <- findBestWith (head bs) subProducts
    findBests (tail bs) keptProducts


-- | Private:

findBestWith :: (Monad m, IObservable p [Flow]) => BestOfParam -> [p] -> EvalProd m ([p], [Flow])
findBestWith params@BestOfParam{..} products =
    let proxies = fmap (`shiftObs` shift) products
        bests = findBestsImpl (evalObs . fst) params (zip proxies products)
    in first (fmap snd) <$> bests

findBestsImpl :: (Monad m) => (p -> EvalProd m [Flow]) -> BestOfParam -> [p] -> EvalProd m ([p], [Flow])
findBestsImpl evalP BestOfParam{..} subProducts = do
    evals <- forM subProducts $ \p -> do
        flows <- evalP p
        converted <- mapM (compound refDate <=< convert refStock) flows
        pure ((p, flows), sum $ fmap flow converted)
    let bests = fst <$> sortBy (compare `on` snd) evals
    pure $ second concat (unzip bests)



