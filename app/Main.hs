{-# LANGUAGE RecordWildCards #-}

module Main where

import           Domain.Cart                    ( CartExpiration(..) )
import qualified Http.Clients.Payments         as P
import           Http.Server                    ( runServer )
import           Resources
import qualified Programs.Checkout             as PC
import qualified Services.Brands               as SB
import qualified Services.Items                as SI
import qualified Services.Orders               as SO
import qualified Services.ShoppingCart         as SC
import           Services

main :: IO ()
main = do
  Res {..} <- mkResources
  brands   <- SB.mkBrands psql
  items    <- SI.mkItems psql
  cart     <- SC.mkShoppingCart redis items exp
  orders   <- SO.mkOrders psql
  client   <- P.mkPaymentClient
  checkout <- PC.mkCheckout client cart orders
  runServer (Services brands cart items orders client)
  where exp = CartExpiration (30 * 60)
