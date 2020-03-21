{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Domain.Cart                    ( CartExpiration(..) )
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
  cart   <- SC.mkShoppingCart (redis res) items exp
  runServer (Services brands items)
  where exp = CartExpiration (30 * 60)
