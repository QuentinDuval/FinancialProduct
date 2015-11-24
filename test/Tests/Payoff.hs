{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
module Tests.Payoff (
    runPayoffTests,
) where


import Control.Monad.Identity
import Observable
import Payoff
import Prelude hiding (ifThenElse)
import Test.HUnit
import Tests.EvalUtils
import Tests.MarketDataSample
import Utils.Syntax


-- | All tests

runPayoffTests :: Test
runPayoffTests = TestList [tangibleTest, scalingTest, conditionalTest, allOfTest, bestOfTest]


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
    checkEval testDate      "Evaluation"  flows payoff
    assertEqual             "Dependecies" depends (getDeps payoff)
    checkFixing testDate    "Fixing test" fixed payoff
    checkEval testDate      "Consistency" flows fixed


-- | Test cases

tangibleTest :: Test
tangibleTest = runSuccess baseTest {
    payoff  = trn 12 today "SILV",
    flows   = [Flow 12 today (Stock "SILV")],
    fixed   = trn 12 today "SILV"
}

scalingTest :: Test
scalingTest = runSuccess baseTest {
    payoff  = scale (rate "EURIBOR3M" today * stock "GOLD" today) (trn 20 today "SILV"),
    depends = mempty { stockDeps = [("GOLD", today)], rateDeps = [("EURIBOR3M", today)] },
    flows   = [Flow 15 today (Stock "SILV")],
    fixed   = trn 15 today "SILV"
}

conditionalTest :: Test
conditionalTest = runSuccess baseTest {
    payoff  = if stock "GOLD" today .<. cst 10.0 then trn 12 today "SILV"
                                                 else trn 10 today "GOLD",
    depends = mempty { stockDeps = [("GOLD", today)] },
    flows   = [Flow 10 today (Stock "GOLD")],
    fixed   = trn 10 today "GOLD"
}

allOfTest :: Test
allOfTest = runSuccess baseTest {
    payoff  = allOf [ scale (stock "GOLD" today) (trn 1 today "GOLD")
                    , scale (stock "SILV" today) (trn 1 today "SILV") ],
    depends = mempty { stockDeps = [("GOLD", today), ("SILV", today)] },
    flows   = [Flow 15 today (Stock "GOLD"), Flow 11 today (Stock "SILV")],
    fixed   = allOf [trn 15 today "GOLD", trn 11 today "SILV"]
}

-- TODO: fails miserably:
-- Best of dependencies does not work... you need the deps of all tangibles...
-- (best is maybe to run the best of, and get deps from monad)

bestOfTest :: Test
bestOfTest = runSuccess baseTest {
    payoff  = bestOf [trn 1 today "SILV", trn 1 today "GOLD"] `withEvalOn` (Stock "USD", today),
    depends = mempty { stockDeps = [("EUR", today), ("GOLD", today), ("USD", today)] },
    flows   = [Flow 1 today (Stock "GOLD")],
    fixed   = trn 1 today "GOLD"
}




