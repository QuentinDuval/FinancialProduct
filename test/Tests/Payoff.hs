{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
module Tests.Payoff (
    runPayoffTests,
) where


import Control.Monad.Identity
import Data.Monoid
import Observable
import Payoff
import Prelude hiding (ifThenElse)
import Test.HUnit
import Tests.EvalUtils
import Tests.MarketDataSample
import Utils.Syntax


-- | All tests

runPayoffTests :: Test
runPayoffTests = TestList
    [ tangibleTest, giveTest, scalingTest, scalingFailTest
    , conditionalTest, allOfTest, bestOfTest, fancyProductTest ]


-- | Fixture

data PayoffTest = PayoffTest {
    testDate :: FinDate,
    payoff   :: FinProduct,
    depends  :: ObsDependencies,
    fixed    :: FinProduct,
    flows    :: [Flow]
} deriving (Show, Eq, Ord)

today :: FinDate
today = fromGregorian 2015 22 21

baseTest :: PayoffTest
baseTest = PayoffTest { testDate = today, payoff = Empty, depends = mempty, fixed = Empty, flows = mempty }

runSuccess :: PayoffTest -> Test
runSuccess PayoffTest{..} = TestCase $ do
    checkEval   "Evaluation"  flows payoff
    assertEqual "Dependecies" depends (getDeps payoff)
    checkFixing "Fixing test" fixed payoff
    checkEval   "Consistency" flows fixed

runPartial :: PayoffTest -> Test
runPartial PayoffTest{..} = TestCase $ do
    checkEvalFail "Evaluation"  payoff
    assertEqual   "Dependecies" depends (getDeps payoff)
    checkFixing   "Fixing test" fixed payoff
    checkEvalFail "Consistency" fixed
    -- TODO: a call to evalKnownFlows that checks against the flow

-- TODO: test "shiftObs" as well (and on the tangibles!)


-- | Test cases

tangibleTest :: Test
tangibleTest = runSuccess baseTest {
    payoff  = recv 12 today "SILV",
    flows   = [Flow 12 today (Stock "SILV")],
    fixed   = recv 12 today "SILV"
}

giveTest :: Test
giveTest = runSuccess baseTest {
    payoff  = give (recv 12 today "SILV"),
    flows   = [Flow (-12) today (Stock "SILV")],
    fixed   = send 12 today "SILV"
}

scalingTest :: Test
scalingTest = runSuccess baseTest {
    payoff  = scale (rate "EURIBOR3M" today * stock "GOLD" today) (recv 20 today "SILV"),
    depends = mempty { stockDeps = [("GOLD", today)], rateDeps = [("EURIBOR3M", today)] },
    flows   = [Flow 15 today (Stock "SILV")],
    fixed   = recv 15 today "SILV"
}

scalingFailTest :: Test
scalingFailTest = runPartial baseTest {
    payoff  = scale (rate "UNKNOWN" today + stock "USD" today) (recv 1 today "SILV"),
    depends = mempty { stockDeps = [("USD", today)], rateDeps = [("UNKNOWN", today)] },
    flows   = [],
    fixed   = scale (rate "UNKNOWN" today + cst 1.0) (recv 1 today "SILV")
}

conditionalTest :: Test
conditionalTest = runSuccess baseTest {
    payoff  = if stock "GOLD" today .<. cst 10.0 then recv 12 today "SILV"
                                                 else recv 10 today "GOLD",
    depends = mempty { stockDeps = [("GOLD", today)] },
    flows   = [Flow 10 today (Stock "GOLD")],
    fixed   = recv 10 today "GOLD"
}

allOfTest :: Test
allOfTest = runSuccess baseTest {
    payoff  = allOf [ scale (stock "GOLD" today) (recv 1 today "GOLD")
                    , scale (stock "SILV" today) (send 1 today "SILV") ],
    depends = mempty { stockDeps = [("GOLD", today), ("SILV", today)] },
    flows   = [Flow 15 today (Stock "GOLD"), Flow (-11) today (Stock "SILV")],
    fixed   = allOf [recv 15 today "GOLD", send 11 today "SILV"]
}

-- TODO: fails miserably:
-- Best of dependencies does not work... you need the deps of all tangibles + the eval currency as well

bestOfTest :: Test
bestOfTest = runSuccess baseTest {
    payoff  = bestOf [recv 1 today "SILV", recv 1 today "GOLD"] `withEvalOn` (Stock "USD", today),
    depends = mempty { stockDeps = [("EUR", today), ("GOLD", today), ("USD", today)] },
    flows   = [Flow 1 today (Stock "GOLD")],
    fixed   = recv 1 today "GOLD"
}

-- TODO: add bestOf with proxy at different dates
-- Dependencies are even harder to get right

fancyProduct :: FinDate -> FinProduct
fancyProduct futureDate =
    scale (cst 5 + stock "GOLD" today / stock "USD" today) $
        allOf [
            recv 1 today "EUR",
            if stock "EUR" futureDate .>. stock "EUR" today
                then recv 1 futureDate "EUR"
                else send 2 futureDate "USD"
        ]

fixedFancyProduct :: FinDate -> FinProduct
fixedFancyProduct futureDate =
    scale (cst 20) $
        allOf [
            recv 1 today "EUR",
            recv 1 futureDate "EUR"
        ]

fancyProductTest :: Test
fancyProductTest =
    let futureDate = today `addDay` 10
    in runSuccess baseTest {
        payoff = fancyProduct futureDate,
        depends = mempty { stockDeps = [("EUR", futureDate),("EUR", today),("GOLD", today),("USD", today)]},
        flows = [Flow 20 today (Stock "EUR"), Flow 20 futureDate (Stock "EUR")],
        fixed = fixedFancyProduct futureDate
    }


