{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Payments.Test
  ( paymentTests
  )
where

import           Data.UUID                      ( fromText )
import qualified Data.UUID.V4                  as UUID
import           Domain.Checkout
import           Domain.Item
import           Domain.Payment
import           Domain.User
import           Hedgehog
import qualified Http.Clients.Payments         as P
import           Refined
import           Refined.Instances              ( )

prop_payments :: Property
prop_payments = withTests (1 :: TestLimit) payments

payments :: Property
payments = property $ do
  client <- evalIO P.mkPaymentClient
  evalIO $ putStrLn "Making payment request"
  uuid <- evalIO UUID.nextRandom
  pid  <- evalIO $ P.processPayment client (payment uuid)
  (Just pid) === (PaymentId <$> fromText "6ff33dbc-6b8c-11ea-bc55-0242ac130003")
 where
  cn   = $$(refineTH "Gabriel Volpe") :: CardNamePred
  cu   = $$(refineTH 1111444477772222) :: CardNumberPred
  ce   = $$(refineTH 1204) :: CardExpirationPred
  cv   = $$(refineTH 123) :: CardCVVPred
  card = Card (CardName cn) (CardNumber cu) (CardExpiration ce) (CardCVV cv)
  payment uid = Payment (UserId uid) (Money 500) card

paymentTests :: Group
paymentTests = $$(discover)
