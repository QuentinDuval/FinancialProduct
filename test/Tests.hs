module Tests (
    runAllTests,
) where

import Test.HUnit
import Tests.Observable
import Tests.Payoff


runAllTests :: IO Counts
runAllTests = runTestTT $ TestList
    [ runObservableTests, runPayoffTests ]

