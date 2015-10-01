module FinProduct where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Lens
import Data.Time

import Flow
import IndexMonad
import MarketData



-- | Vocabulary to describe financial products

newtype Instrument
    = Instrument { instrumentLabel :: String }
    deriving (Show, Eq, Ord)

data FinProduct
    = Tangible  FlowDate Instrument
    | Scale     Quantity FinProduct
    | AllOf     [FinProduct]
    | IfThen    Predicate FinProduct
    | Empty


cst :: (Real a) => a -> Quantity
cst = return . realToFrac

var :: String -> Quantity
var = evalIndex . FI

trn :: Double -> FlowDate -> String -> FinProduct
trn qty date instr = scale (pure qty) (Tangible date (Instrument instr))

scale :: Quantity -> FinProduct -> FinProduct
scale = Scale

choice :: Predicate -> FinProduct -> FinProduct -> FinProduct
choice p a b = mconcat [IfThen p a, IfThen (not <$> p) b]

instance Monoid FinProduct where
    mempty = Empty
    mappend a b = AllOf [a, b]
    mconcat = AllOf


-- | Evaluation of the production of financial products

evalProduct :: FinProduct -> IndexMonad [Flow]
evalProduct Empty           = return []
evalProduct (Tangible d s)  = return [Flow 1 d (instrumentLabel s)]
evalProduct (AllOf ps)      = concat <$> mapM evalProduct ps
evalProduct (IfThen p f)    = do
    v <- p
    if v then evalProduct f
         else return []
evalProduct (Scale q f)     = do
    v <- q
    fs <- evalProduct f
    return $ over flow (*v) <$> fs













