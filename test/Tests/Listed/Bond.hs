module Tests.Listed.Bond (
    runBondTests,
) where


import Control.Monad.Identity
import Observable
import Listed.Bond
import Payoff
import Test.HUnit
import Tests.EvalUtils
import Tests.MarketDataSample


-- | All tests

runBondTests :: Test
runBondTests = TestList [ fixedBondTest, floatingBondTest ]


-- | Fixture

testBond :: (FinDate -> ObsQuantity) -> FinDate -> FinProduct
testBond couponRate t = buyBond bondInfo periods
    where
        periods  = PeriodInfo { startDate = t, period = 10,      periodCount = 3 }
        bondInfo = BondInfo   { nominal = 10,  currency = "EUR", couponRate = couponRate }

checkEvalFlow :: String -> [Flow] -> FinProduct -> Assertion
checkEvalFlow = checkEvalWith ((fmap.fmap) (roundFlow 2) . evalObs)


-- | Test cases

fixedBondTest :: Test
fixedBondTest =
    let payoff = testBond (const $ stockRate "EUR" "USD" today * cst 0.1) today
        flows = [Flow 10     today             (Stock "EUR"),
                 Flow (-1.1) (addDay today 10) (Stock "EUR"),
                 Flow (-1.1) (addDay today 20) (Stock "EUR"),
                 Flow (-10)  (addDay today 30) (Stock "EUR")]
    in TestCase $ checkEvalFlow "Constant values" flows payoff

floatingBondTest :: Test
floatingBondTest =
    let payoff = testBond (\t -> stockRate "EUR" "USD" t * cst 0.1) today
        flows = [Flow 10     today             (Stock "EUR"),
                 Flow (-1.2) (addDay today 10) (Stock "EUR"),
                 Flow (-1.3) (addDay today 20) (Stock "EUR"),
                 Flow (-10)  (addDay today 30) (Stock "EUR")]
    in TestCase $ checkEvalFlow "Floating values" flows payoff


