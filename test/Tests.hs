module Tests (
    runAllTests,
) where

import Test.HUnit
import Tests.Flow
import Tests.Listed.Bond
import Tests.Listed.AsianOption
import Tests.Listed.BestOfOption
import Tests.Listed.EuropeanOption
import Tests.Observable
import Tests.Payoff


runAllTests :: IO Counts
runAllTests = runTestTT $ TestList
    [ TestLabel "Observable"        runObservableTests
    , TestLabel "Flow"              runFlowTests
    , TestLabel "Payoff"            runPayoffTests
    , TestLabel "Listed.Bond"       runBondTests
    , TestLabel "Listed.AsianOpt"   runAsianOptionTests
    , TestLabel "Listed.EurOpt"     runEuropeanOptionTests
    , TestLabel "Listed.BestOfOpt"  runBestOfOptionTests]

