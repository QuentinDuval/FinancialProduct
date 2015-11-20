module Main where

import Control.Monad
--import Control.Monad.Identity
--import Data.Monoid
--import Eval
--import Listed.Bond
--import Listed.Option
--import Prelude hiding (ifThenElse)
--import Observable
--import Payoff.Product
--import TestMarketData
--import Utils.Foldable
--import Utils.Monad
--import Utils.Syntax

import Utils.Time
import Test.HUnit
import System.Exit (exitFailure)

--import Distribution.TestSuite

--
--
--
--mds1 :: FinDate -> TestMarketData
--mds1 t = initMds    [(Stock "GOLD"     , const 15.8)
--                    ,(Stock "SILV"     , const 11.3)
--                    ,(Stock "USD"      , const 1.0)
--                    ,(Stock "EUR"      , \t -> 1.1 + 0.1 * sin (toDayCount t) )]
--                    [(Rate "EURIBOR3M" , const 0.05)
--                    ,(Rate "LIBOR"     , const 0.06)]




testStock t =
    let
    in TestCase $ assertEqual "Boom Boom Boom" True False


main :: IO ()
main = do
    t <- utctDay <$> getCurrentTime
    r <- runTestTT $ TestList [testStock t]
    print r
    when (failures r >= 0) exitFailure


--main :: IO ()
--main = do
----    undefined
--    t <- utctDay <$> getCurrentTime
--    let tests = TestList [TestLabel "testStock" (testStock t)]
--    print =<< showCounts <$> runTestTT tests
    -- TODO: Errors are not reported but are in the logs!

--    putStrLn "\nTest of fixing of observables:"
--    let testObs obsValue = runIdentity $ resultWithEnv (testMdsAccess $ mds1 t) (fixing obsValue)
--    mapM_ print $ testObs <$> [cst 1.0, stock "EUR" t, rate "EURIBOR3M" t, stock "UNKNOWN" t, rate "UNKNOWN" t,
--                               cst 1.0 + stock "EUR" t, stock "EUR" t * rate "EURIBOR3M" t, stockRate "EUR" "UNKNOWN" t]
--    mapM_ print $ testObs <$> [cst 1.0 .<. stock "EUR" t, stock "EUR" t .==. rate "EURIBOR3M" t,
--                               (stock "GOLD" t .>. stock "SILV" t) .&&. cst True .||. cst False]

