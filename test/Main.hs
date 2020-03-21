{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Data.Functor                   ( void )
import           Hedgehog
import           Payments.Test

main :: IO ()
main = do
  putStrLn "No tests yet"
  --void $ checkParallel paymentTests
