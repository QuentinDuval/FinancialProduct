module Tests.EvalUtils where

import Control.Monad.Identity
import Eval
import Observable
import Test.HUnit
import Tests.MarketDataSample



checkEvalWith :: (Eq b, Show b) => (a -> EvalProd Identity b) -> String -> b -> a -> Assertion
checkEvalWith evalObs str expected expression =
    let mdsAccess = testMdsAccess mds
        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (evalObs obsValue)
    in assertEqual str (Done expected) (testFct expression)

checkEval :: (IObservable a b, Eq b, Show b) => String -> b -> a -> Assertion
checkEval = checkEvalWith evalObs

checkFixing :: (IObservable a b, Eq a, Show a) => String -> a -> a -> Assertion
checkFixing = checkEvalWith fixing

checkEvalFail :: (IObservable a b, Eq b, Show b) => String -> a -> Assertion
checkEvalFail str expression =
    let mdsAccess = testMdsAccess mds
        testFct obsValue = runIdentity $ resultWithEnv mdsAccess (evalObs obsValue)
    in assertEqual str (Fail "No results found") (testFct expression)

runFixing :: (IObservable a b) => a -> a
runFixing obsValue = r
    where (Done r) = runIdentity $ resultWithEnv (testMdsAccess mds) (fixing obsValue)
