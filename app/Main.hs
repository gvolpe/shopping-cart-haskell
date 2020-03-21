{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Domain.Cart                    ( CartExpiration(..) )
import qualified Http.Clients.Payments         as P
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
  client <- P.mkPaymentClient
  runServer (Services brands items client)
 where
  exp  = CartExpiration (30 * 60)
