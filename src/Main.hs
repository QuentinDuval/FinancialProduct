module Main (
    main
) where


import qualified Bond
import Data.Time
import EvalMonad
import FinProduct
import MarketData
import MonadUtils


-- | Simple test products

prod1 :: UTCTime -> FinProduct
prod1 t =
    scale (var "USD/EUR") $
        mconcat [
            scale   (var "EURIBOR3M" * cst 0.33)    (trn 120 t "EUR"),
            scale   (var "GOLD" + var "USD/EUR")    (trn 0.9 t "USD"),
            eitherP (var "GOLD" .>. cst 10.0)       (trn 12 t "GOLD") (trn 3 t "SILV"),
            eitherP (var "GOLD" .<. var "USD/EUR")  (trn 17 t "GOLD") (trn 5 t "SILV")]

prod2 :: UTCTime -> FinProduct
prod2 t =
    let periods = Bond.PeriodInfo {
            Bond.startDate = t,
            Bond.gap = 10,
            Bond.periodCount = 3 }
        bond = Bond.BondInfo {
            Bond.nominal = 10,
            Bond.currency = "EUR",
            Bond.rate = var "EURIBOR3M" * var "USD/EUR" * cst 0.05 }
    in Bond.buy bond periods


-- | Two test market data sets

mds1 :: MarketData
mds1 = indexes [(FI "USD/EUR"   , 2.35)
               ,(FI "GOLD"      , 15.8)
               ,(FI "EURIBOR3M" , 0.98)]

mds2 :: MarketData
mds2 = indexes [(FI "USD/EUR"   , 2.07)
               ,(FI "GOLD"      , 1.58)
               ,(FI "EURIBOR3M" , 1.22)]


-- TODO - Add dates to the indexes as well (not necessarily correlated with the dates of the flows)
-- Find a way to add the dates for indices:
-- * A map String -> (UTCTime -> Double) might work (but does not allow side effects)
-- * Then to do a simulation, do provide several MarketData generated separately?
-- * Have a similar map for the volatilities?

-- TODO: The description of the financial product is too entangled with the monad?

-- TODO: Add a "fixing" function that just maps the "var (\r -> e)" to "const (var r)"
-- => somehow we could transform the product given some indices

-- TODO - Bring parallelism in the monad as well


-- | Run tests

main :: IO ()
main = do
    t <- getCurrentTime
    mapM_ print $ do
        prod <- [prod1, prod2] <*> [t]
        mds  <- [mds1, mds2]
        return $ runIndex (evalProduct prod) mds

