{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Data.Functor                   ( void )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen

prop_tap_io :: Property
prop_tap_io = property $ do
  x <- forAll Gen.alpha
  y <- evalIO $ pure x
  x === y

tapTests :: Group
tapTests = $$(discover)

main :: IO ()
main = do
  void $ checkParallel tapTests
