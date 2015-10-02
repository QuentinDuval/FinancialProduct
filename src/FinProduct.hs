module FinProduct where

import Control.Applicative
import Control.Monad
import Control.Lens

import EvalMonad
import Flow
import MarketData
import MonadUtils
import TimeUtils


-- | Vocabulary to describe financial products

newtype Instrument = Instrument { instrumentLabel :: String }
    deriving (Show, Eq, Ord)

data FinProduct
    = Tangible  FinDate Instrument
    | Scale     Quantity FinProduct
    | AllOf     [FinProduct]
    | FirstOf   [(Predicate, FinProduct)]   -- ^ First first matching predicate
    | Empty


stock, rate :: String -> FinDate -> Quantity
stock = evalVar . Stock
rate  = evalVar . Rate

stockRate :: String -> String -> FinDate -> Quantity
stockRate s1 s2 t = (/) <$> stock s1 t <*> stock s2 t

trn :: Double -> FinDate -> String -> FinProduct
trn qty date instr = scale (pure qty) (Tangible date (Instrument instr))

scale :: Quantity -> FinProduct -> FinProduct
scale = Scale

ifThen :: Predicate -> FinProduct -> FinProduct
ifThen p a = eitherP p a Empty

eitherP :: Predicate -> FinProduct -> FinProduct -> FinProduct
eitherP p a b = FirstOf [(p, a), (pure True, b)]

instance Monoid FinProduct where
    mempty = Empty
    mappend x (AllOf xs) = AllOf (x:xs)
    mappend (AllOf xs) x = AllOf (x:xs)
    mappend a b = AllOf [a, b]
    mconcat = AllOf


-- | Evaluation of the production of financial products

evalProduct :: FinProduct -> EvalMonad [Flow]
evalProduct Empty           = return []
evalProduct (Tangible d i)  = return [Flow 1 d (instrumentLabel i)]
evalProduct (AllOf ps)      = concat <$> mapM evalProduct ps
evalProduct (FirstOf ps)    = do
    p <- findM fst ps
    maybe (return []) (evalProduct . snd) p
evalProduct (Scale qty p)   = do
    val <- qty
    flows <- evalProduct p
    return $ over flow (* val) <$> flows













