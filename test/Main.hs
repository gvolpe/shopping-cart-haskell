module Main where

import           Data.Functor                   ( void )
import           Hedgehog
import           Utils.TextTest

main :: IO ()
main = do
  void $ checkParallel utilTextTests
  --void $ checkParallel paymentTests
