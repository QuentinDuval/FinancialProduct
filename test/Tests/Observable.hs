module Tests.Observable (
    runObservableTests,
) where

import Control.Monad.Identity
import Observable
import Payoff
import Test.HUnit
import Tests.EvalUtils
import Tests.MarketDataSample



-- | All tests

runObservableTests :: Test
runObservableTests =
    let t = fromGregorian 2015 22 21 -- TODO: get rid of this
    in TestList [quantityFixing t, predicateFixing t, dependenciesTest t]


-- | Quantity related tests

quantityFixing :: FinDate -> Test
quantityFixing t = TestCase $ do
    checkFixing "Constant quantity" (CstQuantity 1.0)                 (cst 1.0)
    checkFixing "Rate quantity"     (CstQuantity 0.05)                (rate "EURIBOR3M" t)
    checkFixing "Moving stock"      (CstQuantity 1.0133461247168631)  (stock "EUR" t)
    checkFixing "Combining const"   (CstQuantity 3.1)                 (cst 2 * stock "USD" t + cst 1.1)
    checkFixing "Combining vars"    (CstQuantity 0.05)                (rate "EURIBOR3M" t * stock "USD" t)
    checkFixing "Unknown rate"      (RateObs "UNKNOWN" t)             (rate "UNKNOWN" t)
    checkFixing "Unknown stock"     (StockObs "UNKNOWN" t)            (stock "UNKNOWN" t)
    checkFixing "Partial fixing"    (CombineQty Mult [CstQuantity 1.0, Transf Inv (StockObs "UNKNOWN" t)])
                                    (stockRate "USD" "UNKNOWN" t)


-- | Predicate related tests

predicateFixing :: FinDate -> Test
predicateFixing t = TestCase $ do
        checkFixing "Constant boolean"  (CstBool True)        (cst 3.0 .>. cst 2.0)
        checkFixing "Compare quantity"  (CstBool True)        (cst 1.0 .<. stock "EUR" t)
        checkFixing "Equate quantity"   (CstBool False)       (stock "EUR" t .==. rate "EURIBOR3M" t)
        checkFixing "Combining more"    (CstBool True)        ((stock "GOLD" t .>. stock "SILV" t) .&&. CstBool True .||. CstBool False)
        checkFixing "Unknown source"    (QuantityRel IsLTE [CstQuantity 1.0 , StockObs "UNKNOWN" t])
                                          (stock "USD" t .<=. stock "UNKNOWN" t)


-- | Dependency declaration tests

dependenciesTest :: FinDate -> Test
dependenciesTest t =
    let expectedDeps = ObsDependencies
            { stockDeps = [("EUR", t), ("USD", t), ("USD", addDay t 2)]
            , rateDeps  = [("EURIBOR3M", addDay t 2)] }
        obsQuantity  = cst 2 * stock "EUR" t * stock "USD" t + shiftObs (cst 1.1 * rate "EURIBOR3M" t + stock "USD" t) 2
        obsPredicate = (stock "EUR" t .>. stock "USD" t) .||. shiftObs ((rate "EURIBOR3M" t + stock "USD" t) .==. cst 2.0) 2
    in TestCase $ do
        assertEqual "Empty dependencies"    (ObsDependencies [] []) (getDeps (CstQuantity 1.0))
        assertEqual "Quantity expression"   expectedDeps            (getDeps obsQuantity)
        assertEqual "Predicate expression"  expectedDeps            (getDeps obsPredicate)


