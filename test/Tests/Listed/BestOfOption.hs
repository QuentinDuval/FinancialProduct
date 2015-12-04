module Tests.Listed.BestOfOption (
    runBestOfOptionTests,
) where

import Control.Monad.Identity
import Observable
import Listed.Option.BestOf
import Payoff
import Test.HUnit
import Tests.EvalUtils
import Tests.MarketDataSample


-- | All tests

runBestOfOptionTests :: Test
runBestOfOptionTests = TestList [ inTheMoneyTest, outOfTheMoneyTest ]


-- | Fixture

testBestOfOption :: Double -> FinDate -> FinProduct
testBestOfOption strike t = bestOfOption compositeOpt t
    where
        optHeader    = OptionHeader { maturity = 5, premium = send 10 t "USD" }
        optBody1     = OptionBody   { strike = strike, quantity = cst 27, buyInstr = "GOLD", sellInstr = "USD" }
        optBody2     = OptionBody   { strike = strike, quantity = cst 27, buyInstr = "GOLD", sellInstr = "EUR" }
        compositeOpt = CompositeOption optHeader [optBody1, optBody2]


-- | Test cases

inTheMoneyTest :: Test
inTheMoneyTest =
    let payoff = testBestOfOption 10 today
        flows = [Flow (-10)  today             (Stock "USD"),
                 Flow 27     (addDay today 5)  (Stock "GOLD"),
                 Flow (-270) (addDay today 5)  (Stock "USD")]
    in TestCase $ checkEval "Best of option" flows payoff

outOfTheMoneyTest :: Test
outOfTheMoneyTest =
    let payoff = testBestOfOption 20 today
        flows = [Flow (-10)  today             (Stock "USD")]
    in TestCase $ checkEval "Best of option" flows payoff


