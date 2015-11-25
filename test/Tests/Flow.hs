module Tests.Flow (
    runFlowTests
) where


import Control.Monad.Identity
import Eval
import Observable
import Payoff.Flow
import TestMarketData
import Test.HUnit
import Tests.EvalUtils
import Utils.Time


-- | All tests

runFlowTests :: Test
runFlowTests = TestList [ compoundTest, convertTest ]

mds :: FinDate -> TestMarketData
mds today = initMds
    [(Stock "USD"      , \t -> 10.0 + fromIntegral (t `diffDays` today))
    ,(Stock "EUR"      , \t -> 10.0 - fromIntegral (t `diffDays` today))
    ,(Stock "CHF"      , const 10.0)]
    [(Rate "EURIBOR3M" , const 0.05)]


-- | Test cases

compoundTest :: Test
compoundTest =
    let today = fromGregorian 2015 22 21
        plus2 = today `addDay` 2
        minus = today `addDay` (-2)
    in TestCase $ do
        checkCompound today "Increasing" (Flow 10 today (Stock "USD")) (Flow 12 plus2 (Stock "USD"))
        checkCompound today "Decreasing" (Flow 15 today (Stock "EUR")) (Flow 12 plus2 (Stock "EUR"))
        checkCompound today "Stable val" (Flow 12 today (Stock "CHF")) (Flow 12 plus2 (Stock "CHF"))
        checkCompound today "Same date"  (Flow 12 today (Stock "USD")) (Flow 12 today (Stock "USD"))
        checkCompound today "To future"  (Flow 15 today (Stock "USD")) (Flow 12 minus (Stock "USD"))

convertTest :: Test
convertTest =
    let today = fromGregorian 2015 22 21
        plus2 = today `addDay` 2
    in TestCase $ do
        checkConvert today "Same stock" (Stock "USD") (Flow 12 today (Stock "USD")) (Flow 12 today (Stock "USD"))
        checkConvert today "Same value" (Stock "EUR") (Flow 12 today (Stock "EUR")) (Flow 12 today (Stock "USD"))
        checkConvert today "Increasing" (Stock "EUR") (Flow 18 plus2 (Stock "EUR")) (Flow 12 plus2 (Stock "USD"))
        checkConvert today "Decreasing" (Stock "USD") (Flow  8 plus2 (Stock "USD")) (Flow 12 plus2 (Stock "EUR"))


-- | Utils (fixture)

checkCompound :: FinDate -> String -> Flow -> Flow -> Assertion
checkCompound today str expected flow =
    let mdsAccess    = testMdsAccess (mds today)
        compoundFlow = runIdentity $ resultWithEnv mdsAccess (compound today flow)
    in assertEqual str (Done expected) compoundFlow

checkConvert :: FinDate -> String -> Stock -> Flow -> Flow -> Assertion
checkConvert today str stock expected flow =
    let mdsAccess    = testMdsAccess (mds today)
        compoundFlow = runIdentity $ resultWithEnv mdsAccess (convert stock flow)
    in assertEqual str (Done expected) compoundFlow

