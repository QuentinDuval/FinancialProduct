module Tests (
    runAllTests,
) where

import Test.HUnit
import Tests.Observable


runAllTests :: IO Counts
runAllTests = runTestTT $ TestList
    [ runObservableTests ]

