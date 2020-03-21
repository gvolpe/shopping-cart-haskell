{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.UUID.V4                  as UUID
import           Domain.Checkout
import           Domain.Cart                    ( CartExpiration(..) )
import           Domain.Item
import           Domain.Payment
import           Domain.User
import qualified Http.Clients.Payments         as P
import           Http.Server                    ( runServer )
import           Refined
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
  putStrLn "making payment request"
  uuid <- UUID.nextRandom
  pid  <- P.processPayment client (payment uuid)
  print (show pid)
  runServer (Services brands items)
 where
  exp  = CartExpiration (30 * 60)
  cn   = $$(refineTH "Gabriel Volpe") :: CardNamePred
  cu   = $$(refineTH 1111444477772222) :: CardNumberPred
  ce   = $$(refineTH 1204) :: CardExpirationPred
  cv   = $$(refineTH 123) :: CardCVVPred
  card = Card (CardName cn) (CardNumber cu) (CardExpiration ce) (CardCVV cv)
  payment uid = Payment (UserId uid) (Money 500) card
