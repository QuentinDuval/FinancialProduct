module Tests.EvalUtils where

import Control.Monad.Identity
import Eval
import Observable
import Test.HUnit
import Tests.MarketDataSample



checkEval :: (IObservable a b, Eq b, Show b) => FinDate -> String -> b -> a -> Assertion
checkEval t str expected expression =
    let mdsAccess = testMdsAccess (mds t)
        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (evalObs obsValue)
    in assertEqual str (Done expected) (testFct expression)


checkFixing :: (IObservable a b, Eq a, Show a) => FinDate -> String -> a -> a -> Assertion
checkFixing t str expected expression =
    let mdsAccess = testMdsAccess (mds t)
        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (fixing obsValue)
    in assertEqual str (Done expected) (testFct expression)

