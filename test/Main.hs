module Main where

import           Control.Monad                  ( unless )
import           Hedgehog
import           System.Exit
import           Utils.TextTest

main :: IO ()
main = do
  results <- sequence [checkParallel utilTextTests]
  --checkParallel paymentTests
  unless (and results) $ exitFailure
