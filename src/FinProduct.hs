module FinProduct where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Lens
import Data.Time

import Flow
import IndexMonad



-- | Vocabulary to describe financial products

data Product
    = Tangible  FlowDate String
    | Scale     Quantity Product
    | AllOf     [Product]
    | IfThen    Predicate Product
    | Empty

cst :: (Real a) => a -> Quantity
cst = return . realToFrac

var :: String -> Quantity
var = evalIndex . FI

trn :: Double -> FlowDate -> String -> Product
trn qty date instr = scale (pure qty) (Tangible date instr)

scale :: Quantity -> Product -> Product
scale = Scale

choice :: Predicate -> Product -> Product -> Product
choice p a b = AllOf [IfThen p a, IfThen (not <$> p) b]

instance Monoid Product where
    mempty = Empty
    mappend a b = AllOf [a, b]
    mconcat = AllOf


-- | Evaluation of the production of financial products

evalProduct :: Product -> IndexMonad [Flow]
evalProduct Empty           = return []
evalProduct (Tangible d s)  = return [Flow 1 d s]
evalProduct (AllOf ps)      = concat <$> mapM evalProduct ps
evalProduct (IfThen p f)    = do
    v <- p
    if v then evalProduct f
         else return []
evalProduct (Scale q f)     = do
    v <- q
    fs <- evalProduct f
    return $ over flow (*v) <$> fs













