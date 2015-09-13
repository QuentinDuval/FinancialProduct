module Main (
    main
) where


import FinProduct
import IndexMonad


-- | A simple test product

prod :: Product
prod =
    scale (var "USD/EUR") $
        AllOf [scale   (var "EURIBOR3M" * cst 0.33)    (trn 120 "EUR"),
               scale   (var "GOLD" + var "USD/EUR")    (trn 0.9 "USD"),
               choice  (var "GOLD" .>. cst 10.0)       (trn 12 "GOLD") (trn 3 "SILV"),
               choice  (var "GOLD" .<. var "USD/EUR")  (trn 17 "GOLD") (trn 5 "SILV")]


-- | Two test market data sets

mds1 :: Indexes
mds1 = indexes [(FI "USD/EUR"   , 2.35)
               ,(FI "GOLD"      , 15.8)
               ,(FI "EURIBOR3M" , 0.98)]

mds2 :: Indexes
mds2 = indexes [(FI "USD/EUR"   , 2.07)
               ,(FI "GOLD"      , 1.58)
               ,(FI "EURIBOR3M" , 1.22)]


-- | Run tests

main :: IO ()
main = do
    print $ runIndex (evalProduct prod) mds1
    print $ runIndex (evalProduct prod) mds2

