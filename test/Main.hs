module Main where

import Control.Monad
import System.Exit (exitFailure)
import Test.HUnit


import Tests.Observable


main :: IO ()
main = do
    r <- runTestTT $ TestList
        [ runQuantityTests
        , runPredicateTests]
    when (failures r > 0) exitFailure
