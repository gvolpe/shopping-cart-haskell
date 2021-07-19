{-# LANGUAGE OverloadedStrings #-}

module Http.Clients.Payments
  ( PaymentClient(..)
  , mkPaymentClient
  )
where

import           Data.Aeson                     ( toJSON )
import           Domain.Payment
import           Http.Client
import           Network.Wreq                   ( defaults )

data PaymentClient m = PaymentClient
  { processPayment :: Payment -> m PaymentId
  }

mkPaymentClient :: HTTPClient m => PaymentClient m
mkPaymentClient = PaymentClient process'

process' :: HTTPClient m => Payment -> m PaymentId
process' payment =
  let url = "https://payments.free.beeceptor.com/payments"
      ops = defaults
  in  post ops url (toJSON payment)
