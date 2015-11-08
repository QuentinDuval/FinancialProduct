{-# LANGUAGE RecordWildCards #-}
module Payoff.Product.BestOf where

import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Eval
import Payoff.Flow
import Utils.Time


-- | Parameters for the best of behaviors

data BestOfParams
    = BestOfParams { bestCount :: Int, refStock :: Stock, refDate :: FinDate }
    deriving (Show, Read, Eq, Ord)


-- | Algorithms

findBests :: (Monad m) => (p -> EvalProd m [Flow]) -> BestOfParams -> [p] -> EvalProd m ([p], [Flow])
findBests evalP BestOfParams{..} subProducts = do
    evals <- forM subProducts $ \p -> do
        flows <- evalP p
        converted <- mapM (compound refDate <=< convert refStock) flows
        pure ((p, flows), sum $ fmap flow converted)
    let bests = fst <$> sortBy (compare `on` snd) evals
    pure $ second concat (unzip bests)


