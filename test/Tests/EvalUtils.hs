module Tests.EvalUtils where

import Control.Monad.Identity
import Eval
import Observable
import Test.HUnit
import Tests.MarketDataSample



checkEval :: (IObservable a b, Eq b, Show b) => String -> b -> a -> Assertion
checkEval str expected expression =
    let mdsAccess = testMdsAccess mds
        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (evalObs obsValue)
    in assertEqual str (Done expected) (testFct expression)


checkEvalFail :: (IObservable a b, Eq b, Show b) => String -> a -> Assertion
checkEvalFail str expression =
    let mdsAccess = testMdsAccess mds
        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (evalObs obsValue)
    in assertEqual str (Fail "No results found") (testFct expression)


checkFixing :: (IObservable a b, Eq a, Show a) => String -> a -> a -> Assertion
checkFixing str expected expression =
    let mdsAccess = testMdsAccess mds
        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (fixing obsValue)
    in assertEqual str (Done expected) (testFct expression)

