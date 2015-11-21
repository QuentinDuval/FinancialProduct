module Tests.Observable (
    runQuantityTests,
    runPredicateTests,
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


runQuantityTests :: Test
runQuantityTests =
    let t = fromGregorian 2015 22 21
    in TestList
        [ fixingSuccess t "Constant quantity" (CstQuantity 1.0)                 (cst 1.0)
        , fixingSuccess t "Rate quantity"     (CstQuantity 0.05)                (rate "EURIBOR3M" t)
        , fixingSuccess t "Moving stock"      (CstQuantity 1.0133461247168631)  (stock "EUR" t)
        , fixingSuccess t "Combining const"   (CstQuantity 3.1)                 (cst 2 * stock "USD" t + cst 1.1)
        , fixingSuccess t "Combining vars"    (CstQuantity 0.05)                (rate "EURIBOR3M" t * stock "USD" t)
        , fixingSuccess t "Unknown rate"      (RateObs "UNKNOWN" t)             (rate "UNKNOWN" t)
        , fixingSuccess t "Unknown stock"     (StockObs "UNKNOWN" t)            (stock "UNKNOWN" t)
        , fixingSuccess t "Partial fixing"    (CombineQty Mult [CstQuantity 1.0, Transf Inv (StockObs "UNKNOWN" t)])
                                              (stockRate "USD" "UNKNOWN" t)
        ]


runPredicateTests :: Test
runPredicateTests =
    let t = fromGregorian 2015 22 21
    in TestList
        [ fixingSuccess t "Constant boolean"  (CstBool True)        (cst True)
        , fixingSuccess t "Compare quantity"  (CstBool True)        (cst 1.0 .<. stock "EUR" t)
        , fixingSuccess t "Equate quantity"   (CstBool False)       (stock "EUR" t .==. rate "EURIBOR3M" t)
        , fixingSuccess t "Combining more"    (CstBool True)        ((stock "GOLD" t .>. stock "SILV" t) .&&. cst True .||. cst False)
        , fixingSuccess t "Unknown source"    (QuantityRel IsLTE [CstQuantity 1.0 , StockObs "UNKNOWN" t])
                                              (stock "USD" t .<=. stock "UNKNOWN" t)
        ]


fixingSuccess :: (IObservable a b, Eq a, Show a) => FinDate -> String -> a -> a -> Test
fixingSuccess t str expected expression =
    let mdsAccess = testMdsAccess (mds t)
        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (fixing obsValue)
    in TestCase $ assertEqual str (Done expected) (testFct expression)

