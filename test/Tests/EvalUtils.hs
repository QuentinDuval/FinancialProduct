module Tests.EvalUtils (
    checkEvalWith,
    checkEval,
    checkFixing,
    checkEvalFail,
    runFixing,
) where

import Control.Monad.Identity
import Eval
import Observable
import Test.HUnit
import Tests.MarketDataSample


-- | Utils for the eval tests

-- TODO: add tests with and without cache

checkEvalWith :: (Eq b, Show b) => (a -> CachedEval Identity b) -> String -> b -> a -> Assertion
checkEvalWith evalFct str expected = checkEvalImpl evalFct str (Done expected)

checkEval :: (IObservable a b, Eq b, Show b) => String -> b -> a -> Assertion
checkEval = checkEvalWith evalObs

checkFixing :: (IObservable a b, Eq a, Show a) => String -> a -> a -> Assertion
checkFixing = checkEvalWith fixing

checkEvalFail :: (IObservable a b, Eq b, Show b) => String -> a -> Assertion
checkEvalFail str = checkEvalImpl evalObs str missingMarketData

runFixing :: (IObservable a b) => a -> a
runFixing obsValue = r
    where (Done r) = runIdentity $ evalWithCache (testMdsAccess mds) (fixing obsValue)


-- | Private:

checkEvalImpl :: (Eq b, Show b) => (a -> CachedEval Identity b) -> String -> Result b -> a -> Assertion
checkEvalImpl evalFct str expected expression =
    let mdsAccess = testMdsAccess mds
        testFct obsValue = runIdentity $ evalWithCache mdsAccess (evalFct obsValue)
    in assertEqual str expected (testFct expression)
