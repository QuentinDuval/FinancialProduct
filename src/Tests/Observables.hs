module Tests.Observables (
    runTests,
) where

import Control.Monad.Identity
import Eval
import Observable
import Payoff
import TestMarketData
import Test.HUnit
import Utils.Time



mds :: FinDate -> TestMarketData
mds t = initMds
    [(Stock "GOLD"     , const 15.8)
    ,(Stock "SILV"     , const 11.3)
    ,(Stock "USD"      , const 1.0)
    ,(Stock "EUR"      , \t -> 1.1 + 0.1 * sin (toDayCount t) )]
    [(Rate "EURIBOR3M" , const 0.05)
    ,(Rate "LIBOR"     , const 0.06)]


runTests = do
    let t = fromGregorian 2015 22 21
    let testFct obsValue = runIdentity $ resultWithEnv (testMdsAccess $ mds t) (fixing obsValue)
    TestCase $
        assertEqual "Boom Boom Boom" (Done $ CstQuantity 0.05) (testFct $ rate "EURIBOR3M" t)


--    mapM_ print $ testObs <$> [cst 1.0, stock "EUR" t, rate "EURIBOR3M" t, stock "UNKNOWN" t, rate "UNKNOWN" t,
--                               cst 1.0 + stock "EUR" t, stock "EUR" t * rate "EURIBOR3M" t, stockRate "EUR" "UNKNOWN" t]
--    mapM_ print $ testObs <$> [cst 1.0 .<. stock "EUR" t, stock "EUR" t .==. rate "EURIBOR3M" t,
--                               (stock "GOLD" t .>. stock "SILV" t) .&&. cst True .||. cst False]
