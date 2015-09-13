{-# LANGUAGE TemplateHaskell #-}
module Flow where

import Control.Lens


-- | Data structure to describe the production of financial products

data Flow = Flow
    { _flow     :: Double
    , _flowCurr :: String }
    deriving (Show, Eq, Ord)

makeLenses ''Flow

