module Main (
    main
) where


import qualified Listed.Bond as Bond
import qualified Listed.SimpleOption as Opt

import Control.Monad.Identity
import Data.Monoid
import EvalProd
import FinProduct
import MarketData
import Observable
import TestMarketData
import Utils.Monad
import Utils.Time


-- | Simple test products

testP1 :: FinDate -> FinProduct
testP1 t =
    scale (cst 1.0 + stockRate "USD" "EUR" t) $
        mconcat [
            bestOfBy (Stock "USD")                       [trn 120 t "EUR" , trn 120 t "USD"],
            scale    (rate "EURIBOR3M" t + cst 0.33)     (trn 120 t "EUR"),
            scale    (stock "GOLD" t * rate "LIBOR" t)   (trn 0.9 t "USD"),
            eitherP  (stock "GOLD" t .<. cst 10.0)       (trn 12 t "SILV") (trn 10 t "GOLD"),
            eitherP  (stock "GOLD" t .>. stock "SILV" t) (trn 10 t "GOLD") (trn 10 t "SILV")]

testP2 :: FinDate -> FinProduct
testP2 t = scale (stock "UNKNOWN" t) (trn 1 t "USD") <> testP1 t

bond :: (FinDate -> Quantity) -> FinDate -> FinProduct
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


-- TODO: The description of the financial product is too entangled with the monad
-- In fact, we do not need the monad to contain the market data, but just to encapsulate the notion of observable?

-- TODO - Add dates to the indexes as well (not necessarily correlated with the dates of the flows)
-- Find a way to add the dates for indices:
-- * A map String -> (FinDate -> Double) might work (but does not allow side effects)
-- * Then to do a simulation, do provide several MarketData generated separately?
-- * Have a similar map for the volatilities?

-- TODO: Add a "fixing" function that just maps the "var (\r -> e)" to "const (var r)"
-- => somehow we could transform the product given some indices

-- TODO: Add "compression" function (scale could be grouped, etc.)

-- TODO: enhance the notion of time values to look into the past

-- TODO - Bring parallelism in the monad as well


-- | Run tests

main :: IO ()
main = do
    t <- utctDay <$> getCurrentTime
    print (testP1 t)

    putStrLn "\nTest of evaluation of products:"
    let testEval prod mds = runIdentity $ resultWithEnv (testMdsAccess mds) (evalProduct prod)
    mapM_ print $ do
        prod <- [testP1, testP2, bond1, bond2, simpleOption] <*> [t]
        mds  <- [mds1, mds2] <*> [t]
        return $ testEval prod mds

    putStrLn "\nTest of fixing of observables:"
    let testObs obsValue = runIdentity $ resultWithEnv (testMdsAccess $ mds1 t) (fixing obsValue)
    mapM_ print $ testObs <$> [cst 1.0, stock "EUR" t, rate "EURIBOR3M" t, stock "UNKNOWN" t, rate "UNKNOWN" t,
                               cst 1.0 + stock "EUR" t, stock "EUR" t * rate "EURIBOR3M" t, stockRate "EUR" "UNKNOWN" t]
    mapM_ print $ testObs <$> [cst 1.0 .<. stock "EUR" t, stock "EUR" t .==. rate "EURIBOR3M" t,
                               (stock "GOLD" t .>. stock "SILV" t) .&&. cst True .||. cst False]

    putStrLn "\nTest of fixing of products:"
    let testFix prod mds = runIdentity $ resultWithEnv (testMdsAccess mds) (fixProduct prod)
    mapM_ print $ do
        prod <- [testP1, testP2] <*> [t]
        mds  <- [mds1, mds2] <*> [t]
        return $ testFix prod mds


