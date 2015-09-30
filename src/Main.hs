module Main (
    main
) where


import qualified Bond
import Data.Time
import FinProduct
import IndexMonad
import MonadUtils


-- | Simple test products

prod1 :: UTCTime -> FinProduct
prod1 t =
    scale (var "USD/EUR") $
        mconcat [
            scale   (var "EURIBOR3M" * cst 0.33)    (trn 120 t "EUR"),
            scale   (var "GOLD" + var "USD/EUR")    (trn 0.9 t "USD"),
            choice  (var "GOLD" .>. cst 10.0)       (trn 12 t "GOLD") (trn 3 t "SILV"),
            choice  (var "GOLD" .<. var "USD/EUR")  (trn 17 t "GOLD") (trn 5 t "SILV")]

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

mds1 :: Indexes
mds1 = indexes [(FI "USD/EUR"   , 2.35)
               ,(FI "GOLD"      , 15.8)
               ,(FI "EURIBOR3M" , 0.98)]

mds2 :: Indexes
mds2 = indexes [(FI "USD/EUR"   , 2.07)
               ,(FI "GOLD"      , 1.58)
               ,(FI "EURIBOR3M" , 1.22)]


-- TODO - Add dates to the indexes as well (not necessarily correlated with the dates of the flows)
-- Find a way to add the dates for indices: maybe the map is already too concrete (Behaviors?)

-- TODO: Add a "fixing" function that just maps the "var (\r -> e)" to "const (var r)"

-- TODO - Bring parallelism in the monad as well


-- | Run tests

main :: IO ()
main = do
    t <- getCurrentTime
    let [p1, p2] = [prod1, prod2] <*> [t]
    print $ runIndex (evalProduct p1) mds1
    print $ runIndex (evalProduct p1) mds2
    print $ runIndex (evalProduct p2) mds1

