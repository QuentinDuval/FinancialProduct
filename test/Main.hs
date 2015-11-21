module Main where

import Control.Monad
import System.Exit (exitFailure)
import Test.HUnit


import qualified Tests.Observables as Observables


main :: IO ()
main = do
    r <- runTestTT $ TestList [Observables.runTests]
    when (failures r > 0) exitFailure
