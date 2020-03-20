{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Http.Server                    ( runServer )
import           Resources
import qualified Services.Brands               as SB
import qualified Services.Items                as SI
import qualified Services.ShoppingCart         as SC
import           Services

main :: IO ()
main = do
  res    <- mkResources
  brands <- SB.mkBrands (psql res)
  items  <- SI.mkItems (psql res)
  cart   <- SC.mkShoppingCart (redis res) items (30 * 60)
  runServer (Services brands items)
