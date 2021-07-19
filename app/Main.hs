{-# LANGUAGE RecordWildCards #-}

module Main where

import           Domain.Cart                    ( CartExpiration(..) )
import           Http.Clients.Payments          ( mkPaymentClient )
import           Http.Server                    ( runServer )
import           Resources
import           Programs.Checkout              ( mkCheckout )
import           Services.Brands                ( mkBrands )
import           Services.Items                 ( mkItems )
import           Services.Orders                ( mkOrders )
import           Services.ShoppingCart          ( mkShoppingCart )
import           Services

main :: IO ()
main = mkResources >>= \Res {..} ->
  let brands   = mkBrands psql
      items    = mkItems psql
      cart     = mkShoppingCart redis items exp'
      orders   = mkOrders psql
      client   = mkPaymentClient
      checkout = mkCheckout client cart orders
      exp'     = CartExpiration (30 * 60)
  in  runServer (Services brands cart checkout items orders client)
