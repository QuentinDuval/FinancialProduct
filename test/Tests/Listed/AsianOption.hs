module Tests.Listed.AsianOption (
    runAsianOptionTests,
) where

import Control.Monad.Identity
import Observable
import Listed.Option.Asian
import Payoff
import Test.HUnit
import Tests.EvalUtils
import Tests.MarketDataSample


-- | All tests

runAsianOptionTests :: Test
runAsianOptionTests = TestList
    [ inTheMoneyTest, outOfTheMoneyTest, couldNotFixAllTest ]


-- | Fixture

testAsianOption :: Double -> String -> FinDate -> FinProduct
testAsianOption strike instr t = asianOption (SimpleOption optHeader optBody) 1 t
    where
        optHeader   = OptionHeader { maturity = 5, premium = send 10 t "USD" }
        optBody     = OptionBody   { strike = strike, quantity = cst 2, buyInstr = instr, sellInstr = "USD" }


-- | Test cases

inTheMoneyTest :: Test
inTheMoneyTest =
    let payoff = testAsianOption 101 "BNP" today
        flows = [Flow (-10)  today             (Stock "USD"),
                 Flow 2      (addDay today 5)  (Stock "BNP"),
                 Flow (-202) (addDay today 5)  (Stock "USD")]
    in TestCase $ checkEval "Asian option" flows payoff

outOfTheMoneyTest :: Test
outOfTheMoneyTest =
    let payoff = testAsianOption 103 "BNP" today
        flows = [Flow (-10)  today             (Stock "USD")]
    in TestCase $ checkEval "Asian option" flows payoff

couldNotFixAllTest :: Test
couldNotFixAllTest =
    let payoff = testAsianOption 103 "BMW" today
    in TestCase $ checkEvalFail "Asian option" payoff



