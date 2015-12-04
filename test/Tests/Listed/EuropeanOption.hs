module Tests.Listed.EuropeanOption (
    runEuropeanOptionTests,
) where

import Control.Monad.Identity
import Observable
import Listed.Option.European
import Payoff
import Test.HUnit
import Tests.EvalUtils
import Tests.MarketDataSample


-- | All tests

runEuropeanOptionTests :: Test
runEuropeanOptionTests = TestList [ inTheMoneyTest, outOfTheMoneyTest ]


-- | Fixture

eurOption :: Double -> FinDate -> FinProduct
eurOption s t = europeanOption (SimpleOption optHeader optBody) t
    where
        optHeader   = OptionHeader { maturity = 5, premium = send 10 t "USD" }
        optBody     = OptionBody   { strike = s, quantity = cst 27, buyInstr = "GOLD", sellInstr = "USD" }


-- | Test cases

inTheMoneyTest :: Test
inTheMoneyTest =
    let payoff = eurOption 10 today
        flows = [Flow (-10)  today             (Stock "USD"),
                 Flow 27     (addDay today 5)  (Stock "GOLD"),
                 Flow (-270) (addDay today 5)  (Stock "USD")]
    in TestCase $ checkEval "European option" flows payoff

outOfTheMoneyTest :: Test
outOfTheMoneyTest =
    let payoff = eurOption 20 today
        flows = [Flow (-10)  today             (Stock "USD")]
    in TestCase $ checkEval "European option" flows payoff

