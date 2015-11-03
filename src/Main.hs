{-# LANGUAGE RebindableSyntax #-}
module Main (
    main
) where


import qualified Listed.Bond as Bond
import qualified Listed.SimpleOption as Opt

import Control.Monad.Identity
import Data.Monoid
import EvalProd
import MarketData
import Prelude hiding (ifThenElse)
import Observable
import Payoff.FinancialProduct
import TestMarketData
import Utils.Foldable
import Utils.Monad
import Utils.Syntax
import Utils.Time


-- | Simple test products

testP1 :: FinDate -> FinProduct
testP1 t =
    scale (cst 1.0 + stockRate "USD" "EUR" t) $
        mconcat [
            bestOfBy    (Stock "USD")                       [trn 120 t "EUR" , trn 120 t "USD"],
            scale       (rate "EURIBOR3M" t + cst 0.33)     (trn 120 t "EUR"),
            scale       (stock "GOLD" t * rate "LIBOR" t)   (trn 0.9 t "USD"),
            if stock "GOLD" t .<. cst 10.0       then trn 12 t "SILV" else trn 10 t "GOLD",
            if stock "GOLD" t .>. stock "SILV" t then trn 10 t "GOLD" else trn 10 t "SILV"]

testP2 :: FinDate -> FinProduct
testP2 t = scale (stock "UNKNOWN" t) (trn 1 t "USD") <> testP1 t

bond :: (FinDate -> ObsQuantity) -> FinDate -> FinProduct
bond couponRate t = Bond.buy bondInfo periods
    where
        periods  = Bond.PeriodInfo { Bond.startDate = t, Bond.period = 10,      Bond.periodCount = 3 }
        bondInfo = Bond.BondInfo   { Bond.nominal = 10,  Bond.currency = "EUR", Bond.couponRate = couponRate }

bond1, bond2 :: FinDate -> FinProduct
bond1   = bond $ (+) <$> rate "EURIBOR3M" <*> rate "LIBOR"
bond2 t = bond (const $ cst 0.05 * stockRate "EUR" "USD" t) t

simpleOption :: FinDate -> FinProduct
simpleOption t = Opt.create optInfo t
    where
        optInfo = Opt.OptionInfo {
            Opt.premium   = trn 5 t "USD",
            Opt.maturity  = 5,
            Opt.strike    = 10,
            Opt.quantity  = cst 27 * stock "EUR" t,
            Opt.buyInstr  = "GOLD",
            Opt.sellInstr = "USD" }


-- | Two test market data sets

mds1 :: FinDate -> TestMarketData
mds1 t = initMds    [(Stock "GOLD"     , const 15.8)
                    ,(Stock "SILV"     , const 11.3)
                    ,(Stock "USD"      , const 1.0)
                    ,(Stock "EUR"      , \t -> 1.1 + 0.1 * sin (toDayCount t) )]
                    [(Rate "EURIBOR3M" , const 0.05)
                    ,(Rate "LIBOR"     , const 0.06)]

mds2 :: FinDate -> TestMarketData
mds2 t = initMds    [(Stock "GOLD"     , const 1.58)
                    ,(Stock "SILV"     , const 17.3)
                    ,(Stock "USD"      , const 1.0)
                    ,(Stock "EUR"      , \t -> 0.9  + 0.1 * sin (toDayCount t) )]
                    [(Rate "EURIBOR3M" , \t -> 0.05 + 0.01 * sin (toDayCount t / 10) )
                    ,(Rate "LIBOR"     , \t -> 0.06 + 0.01 * cos (toDayCount t / 12) )]

-- TODO: have a function that computes the discounted P&L as of a date, and use it in the BestOf (add the date)
-- TODO: Write a small main loop that reads the market data as input, then ask for pricing of products?
--       And give some example functions to show the things.
-- TODO: Remove date from tangible, and allow to set / shift date
-- TODO: Try to represent the notion of rights? (rights to buy, rights to divident, etc.)
-- TODO: Renamed "FinProduct" to "Flow schedule generator"? Because it is one... or "Payoff"?
-- TODO: Introduct the BestNOf (2 best products out of 3 for example)
-- TODO: Make it easy to do simulation of flows
-- TODO: Apply the discout factors of flows when evaluating "BestOf" => You need the "trend" (financing rate / dividend) + "volatility" in the market
-- TODO: Persist the products with (Show / Read) => plug that in the main
-- TODO: Persist with JSON: https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
-- TODO: Model "Asian Option" => based on the average value of the deal
-- TODO: Model "Best of option" => 3 options at first, one year after, keep the 2 bests, then 1 year after, keep the best, then option
-- TODO: Add "compression" function (scale could be grouped, etc.)
-- TODO: Bring parallelism in the monad as well


-- | Run tests

main :: IO ()
main = do
    t <- utctDay <$> getCurrentTime
    print (testP1 t)

    let i = if True then 1 else 0

    putStrLn "\nTest of evaluation of products:"
    let testEval prod mds f = runIdentity $ resultWithEnv (testMdsAccess mds) (f prod)
    mapM_ print $ do
        prod <- [testP1, testP2, bond1, bond2, simpleOption] <*> [t]
        mds  <- [mds1, mds2] <*> [t]
        f <- [evalObs, evalKnownFlows]
        return $ testEval prod mds f

    putStrLn "\nTest of fixing of observables:"
    let testObs obsValue = runIdentity $ resultWithEnv (testMdsAccess $ mds1 t) (fixing obsValue)
    mapM_ print $ testObs <$> [cst 1.0, stock "EUR" t, rate "EURIBOR3M" t, stock "UNKNOWN" t, rate "UNKNOWN" t,
                               cst 1.0 + stock "EUR" t, stock "EUR" t * rate "EURIBOR3M" t, stockRate "EUR" "UNKNOWN" t]
    mapM_ print $ testObs <$> [cst 1.0 .<. stock "EUR" t, stock "EUR" t .==. rate "EURIBOR3M" t,
                               (stock "GOLD" t .>. stock "SILV" t) .&&. cst True .||. cst False]

    putStrLn "\nTest of fixing of products:"
    let testFix prod mds = runIdentity $ resultWithEnv (testMdsAccess mds) (fixing prod)
    mapM_ print $ do
        prod <- [testP1, testP2] <*> [t]
        mds  <- [mds1, mds2] <*> [t]
        return $ testFix prod mds


