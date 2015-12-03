module Tests (
    runAllTests,
) where

import Test.HUnit
import Tests.Flow
import Tests.Listed.Bond
import Tests.Observable
import Tests.Payoff


runAllTests :: IO Counts
runAllTests = runTestTT $ TestList
    [ TestLabel "Observable"    runObservableTests
    , TestLabel "Flow"          runFlowTests
    , TestLabel "Payoff"        runPayoffTests
    , TestLabel "Listed.Bond"   runBondTests ]

