module Tests.Observable (
    runObservableTests,
) where

import Control.Monad.Identity
import Eval
import Observable
import Payoff
import TestMarketData
import Test.HUnit
import Utils.Time



-- | Fixture (test set)

mds :: FinDate -> TestMarketData
mds t = initMds
    [(Stock "GOLD"     , const 15.8)
    ,(Stock "SILV"     , const 11.3)
    ,(Stock "USD"      , const 1.0)
    ,(Stock "EUR"      , \t -> 1.1 + 0.1 * sin (toDayCount t) )]
    [(Rate "EURIBOR3M" , const 0.05)
    ,(Rate "LIBOR"     , const 0.06)]

runObservableTests :: Test
runObservableTests =
    let t = fromGregorian 2015 22 21
    in TestList [quantityFixing t, predicateFixing t, dependenciesTest t]


-- | Quantity related tests

quantityFixing :: FinDate -> Test
quantityFixing t = TestList
    [ fixingTest t "Constant quantity" (CstQuantity 1.0)                 (cst 1.0)
    , fixingTest t "Rate quantity"     (CstQuantity 0.05)                (rate "EURIBOR3M" t)
    , fixingTest t "Moving stock"      (CstQuantity 1.0133461247168631)  (stock "EUR" t)
    , fixingTest t "Combining const"   (CstQuantity 3.1)                 (cst 2 * stock "USD" t + cst 1.1)
    , fixingTest t "Combining vars"    (CstQuantity 0.05)                (rate "EURIBOR3M" t * stock "USD" t)
    , fixingTest t "Unknown rate"      (RateObs "UNKNOWN" t)             (rate "UNKNOWN" t)
    , fixingTest t "Unknown stock"     (StockObs "UNKNOWN" t)            (stock "UNKNOWN" t)
    , fixingTest t "Partial fixing"    (CombineQty Mult [CstQuantity 1.0, Transf Inv (StockObs "UNKNOWN" t)])
                                       (stockRate "USD" "UNKNOWN" t)]


-- | Predicate related tests

predicateFixing :: FinDate -> Test
predicateFixing t = TestList
        [ fixingTest t "Constant boolean"  (CstBool True)        (cst True)
        , fixingTest t "Compare quantity"  (CstBool True)        (cst 1.0 .<. stock "EUR" t)
        , fixingTest t "Equate quantity"   (CstBool False)       (stock "EUR" t .==. rate "EURIBOR3M" t)
        , fixingTest t "Combining more"    (CstBool True)        ((stock "GOLD" t .>. stock "SILV" t) .&&. cst True .||. cst False)
        , fixingTest t "Unknown source"    (QuantityRel IsLTE [CstQuantity 1.0 , StockObs "UNKNOWN" t])
                                           (stock "USD" t .<=. stock "UNKNOWN" t)]


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


-- | Fixture (utils)

fixingTest :: (IObservable a b, Eq a, Show a) => FinDate -> String -> a -> a -> Test
fixingTest t str expected expression =
    let mdsAccess = testMdsAccess (mds t)
        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (fixing obsValue)
    in TestCase $ assertEqual str (Done expected) (testFct expression)


--evalSuccess :: (IObservable a a, Eq a, Show a) => FinDate -> String -> a -> a -> Test
--evalSuccess t str expected expression =
--    let mdsAccess = testMdsAccess (mds t)
--        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (evalObs obsValue)
--    in TestCase $ assertEqual str (Done expected) (testFct expression)
--
--evalFailure :: (IObservable a a, Eq a, Show a) => FinDate -> String -> a -> Test
--evalFailure t str expression =
--    let mdsAccess = testMdsAccess (mds t)
--        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (evalObs obsValue)
--    in TestCase $ assertEqual str (Fail "") (testFct expression)

