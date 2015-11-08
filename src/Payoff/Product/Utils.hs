{-# LANGUAGE RecordWildCards #-}
module Payoff.Product.Utils where

import Control.Applicative
import Eval
import Observable.Class
import Payoff.Flow
import Payoff.Product.Core
import Utils.Monad


-- | Eval know flows only

evalKnownFlows :: (Monad m) => FinProduct -> EvalProd m [Flow]
evalKnownFlows AllOf{..}       = concatMapM evalKnownFlows subProducts
evalKnownFlows FirstOf{..}     = (findFirstProduct predicates subProducts >>= evalKnownFlows) <|> pure []
evalKnownFlows p               = evalObs p <|> pure []
