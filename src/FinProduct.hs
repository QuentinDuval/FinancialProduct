module FinProduct where

import Control.Applicative
import Control.Arrow
import Control.Monad
import EvalMonad
import Flow
import MarketData
import Utils.Monad
import Utils.Time


-- | Vocabulary to describe financial products

newtype Instrument = Instrument { instrumentLabel :: String }
    deriving (Show, Eq, Ord)

-- TODO: Add a BestOf product, but requires a "reference"
-- TODO: Remove date from tangible, and allow to set / shift date

data FinProduct
    = Tangible  FinDate Instrument
    | Scale     Quantity FinProduct
    | AllOf     [FinProduct]                -- ^ All products are considered
    | FirstOf   [(Predicate, FinProduct)]   -- ^ First first matching predicate
    | Empty


stock, rate :: String -> FinDate -> Quantity
stock = evalVar . Stock
rate  = evalVar . Rate

stockRate :: String -> String -> FinDate -> Quantity    -- TODO: Try to have different kind of rates instead
stockRate s1 s2 t = (/) <$> stock s1 t <*> stock s2 t

trn :: Double -> FinDate -> String -> FinProduct
trn qty date instr = scale (cst qty) (Tangible date (Instrument instr))

scale :: Quantity -> FinProduct -> FinProduct
scale _ Empty           = Empty
scale q (AllOf ps)      = AllOf (scale q <$> ps)
scale q (FirstOf ps)    = FirstOf (second (scale q) <$> ps)
scale q (Scale q' p)    = Scale (q * q') p
scale q p               = Scale q p

send, give :: FinProduct -> FinProduct
send = id
give = scale (cst (-1))

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
    firstMatch <- findM fst ps
    let p = maybe Empty snd firstMatch
    evalProduct p
evalProduct (Scale qty p)   = do
    val <- qty
    flows <- evalProduct p
    return $ overFlow (*val) <$> flows













