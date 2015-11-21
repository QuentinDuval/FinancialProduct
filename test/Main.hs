module Main where

import Control.Monad
import System.Exit (exitFailure)
import Test.HUnit
import Tests


main :: IO ()
main = do
    r <- runAllTests
    when (failures r > 0) exitFailure
