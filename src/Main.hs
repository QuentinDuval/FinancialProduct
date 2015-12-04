{-# LANGUAGE RebindableSyntax #-}
module Main (
    main
) where


import Control.Monad.Identity
import Data.Monoid
import Eval
import Listed.Bond
import Listed.Option
import Prelude hiding (ifThenElse)
import Observable
import Payoff.Product
import TestMarketData
import Utils.Foldable
import Utils.Monad
import Utils.Syntax
import Utils.Time


-- | Simple test products

testP1 :: FinDate -> FinProduct
testP1 t =
    scale (cst 1.0 + stockRate "USD" "EUR" t) $
        allOf [
            bestOf            [recv 120 t "EUR", recv 120 t "USD"]            `withEvalOn` (Stock "USD", t),
            cascadingBestsOf  [recv 120 t "USD", recv 120 t "EUR"] [(1, -2)]  `withEvalOn` (Stock "USD", t),
            scale             (rate "EURIBOR3M" t + cst 0.33)     (recv 120 t "EUR"),
            scale             (stock "GOLD" t * rate "LIBOR" t)   (recv 0.9 t "USD"),
            if stock "GOLD" t .<. cst 10.0       then recv 12 t "SILV" else recv 10 t "GOLD",
            if stock "GOLD" t .>. stock "SILV" t then recv 10 t "GOLD" else recv 10 t "SILV"]

testP2 :: FinDate -> FinProduct
testP2 t = scale (stock "UNKNOWN" t) (recv 1 t "USD") <> testP1 t


-- | Two test market data sets

mds :: FinDate -> TestMarketData
mds t = initMds [(Stock "GOLD"     , const (pure 15.8))
                ,(Stock "SILV"     , const (pure 11.3))
                ,(Stock "USD"      , const (pure 1.0))
                ,(Stock "EUR"      , \t -> pure (1.1 + 0.1 * sin (toDayCount t)) )]
                [(Rate "EURIBOR3M" , const (pure 0.05))
                ,(Rate "LIBOR"     , const (pure 0.06))]


-- TODO: Write a small main loop that reads the market data as input, then ask for pricing of products?
--       And give some example functions to show the things.
-- TODO: Try to represent the notion of rights? (rights to buy, rights to dividend, etc.)
-- TODO: Make it easy to do simulation of flows
-- TODO: Persist the products with (Show / Read) => plug that in the main
-- TODO: Persist with JSON: https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
-- TODO: Bring parallelism in the monad as well


-- | Run tests

main :: IO ()
main = do
    t <- utctDay <$> getCurrentTime
    print (testP1 t)

    putStrLn "\nTest of evaluation of products:"
    let testEval prod mds f = runIdentity $ resultWithEnv (testMdsAccess mds) (f prod)
    mapM_ print $ do
        prod <- [testP1, testP2] <*> [t]
        f <- [evalObs, evalKnownFlows]
        return $ testEval prod (mds t) f

    putStrLn "\nTest of fixing of products:"
    let testFix prod mds = runIdentity $ resultWithEnv (testMdsAccess mds) (fixing prod)
    mapM_ print $ do
        prod <- [testP1, testP2] <*> [t]
        return $ testFix prod (mds t)


