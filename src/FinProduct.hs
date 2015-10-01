module FinProduct where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Lens
import Data.Time

import EvalMonad
import Flow
import MarketData
import MonadUtils



-- | Vocabulary to describe financial products

newtype Instrument
    = Instrument { instrumentLabel :: String }
    deriving (Show, Eq, Ord)

data FinProduct
    = Tangible  FlowDate Instrument
    | Scale     Quantity FinProduct
    | AllOf     [FinProduct]
    | FirstOf   [(Predicate, FinProduct)]   -- ^ First first matching predicate
    | Empty


cst :: (Real a) => a -> Quantity
cst = return . realToFrac

var :: String -> Quantity
var = evalIndex . FI

trn :: Double -> FlowDate -> String -> FinProduct
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
evalProduct (Tangible d s)  = return [Flow 1 d (instrumentLabel s)]
evalProduct (AllOf ps)      = concat <$> mapM evalProduct ps
evalProduct (FirstOf ps)    = do
    p <- findM fst ps
    maybe (return []) (evalProduct . snd) p
evalProduct (Scale q f)     = do
    v <- q
    fs <- evalProduct f
    return $ over flow (*v) <$> fs













