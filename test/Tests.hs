module Tests (
    runAllTests,
) where

import Test.HUnit
import Tests.Flow
import Tests.Observable
import Tests.Payoff


runAllTests :: IO Counts
runAllTests = runTestTT $ TestList
    [ runObservableTests, runFlowTests, runPayoffTests ]

