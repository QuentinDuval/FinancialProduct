module Main (
    main
) where


import qualified Bond
import EvalMonad
import FinProduct
import MarketData
import MonadUtils
import TimeUtils


-- | Simple test products

prod1 :: FinDate -> FinProduct
prod1 t =
    scale (var "USD/EUR" t) $
        mconcat [
            scale   (var "EURIBOR3M" t * cst 0.33)     (trn 120 t "EUR"),
            scale   (var "GOLD" t + var "USD/EUR" t)   (trn 0.9 t "USD"),
            eitherP (var "GOLD" t .>. cst 10.0)        (trn 12 t "GOLD") (trn 3 t "SILV"),
            eitherP (var "GOLD" t .<. var "USD/EUR" t) (trn 17 t "GOLD") (trn 5 t "SILV")]

prod2 :: FinDate -> FinProduct
prod2 t =
    let periods = Bond.PeriodInfo {
            Bond.startDate = t,
            Bond.gap = 10,
            Bond.periodCount = 3 }
        bond = Bond.BondInfo {
            Bond.nominal = 10,
            Bond.currency = "EUR",
            Bond.rate = var "EURIBOR3M" t * var "USD/EUR" t * cst 0.05 }
    in Bond.buy bond periods


-- | Two test market data sets

mds1 :: FinDate -> MarketData
mds1 t = indexes [(FI "USD/EUR"   , \t -> 2.35 + 0.1 * sin (toDayCount t) )
                 ,(FI "GOLD"      , const 15.8)
                 ,(FI "EURIBOR3M" , const 0.98)]

mds2 :: FinDate -> MarketData
mds2 t = indexes [(FI "USD/EUR"   , const 2.07)
                 ,(FI "GOLD"      , const 1.58)
                 ,(FI "EURIBOR3M" , const 1.22)]


-- TODO: The description of the financial product is too entangled with the monad
-- In fact, we do not need the monad to contain the market data, but just to encapsulate the notion of observable?

-- TODO - Add dates to the indexes as well (not necessarily correlated with the dates of the flows)
-- Find a way to add the dates for indices:
-- * A map String -> (FinDate -> Double) might work (but does not allow side effects)
-- * Then to do a simulation, do provide several MarketData generated separately?
-- * Have a similar map for the volatilities?

-- TODO: Add a "fixing" function that just maps the "var (\r -> e)" to "const (var r)"
-- => somehow we could transform the product given some indices

-- TODO - Bring parallelism in the monad as well


-- | Run tests

main :: IO ()
main = do
    t <- getCurrentTime
    mapM_ print $ do
        prod <- [prod1, prod2] <*> [t]
        mds  <- [mds1, mds2] <*> [t]
        return $ withMarketData mds (evalProduct prod)

