{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Programs.Checkout
  ( Checkout(..)
  , mkCheckout
  )
where

import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Retry
import qualified Data.Text                     as T
import qualified Data.UUID                     as UUID
import           Domain.Cart
import           Domain.Checkout
import           Domain.Item
import           Domain.Order
import           Domain.Payment
import           Domain.User
import           Http.Clients.Payments          ( PaymentClient )
import qualified Http.Clients.Payments         as PC
import           Logger
import           Services.Orders                ( Orders )
import qualified Services.Orders               as SO
import           Services.ShoppingCart          ( ShoppingCart )
import qualified Services.ShoppingCart         as SC

data Checkout m = Checkout
  { process :: UserId -> Card -> m OrderId
  }

mkCheckout
  :: (Logger m, MonadIO m, MonadMask m)
  => PaymentClient m
  -> ShoppingCart m
  -> Orders m
  -> m (Checkout m)
mkCheckout p s o = pure $ Checkout (process' p s o)

policy :: Monad m => RetryPolicyM m
policy = limitRetries 3 <> exponentialBackoff 10000

processPayment'
  :: (Logger m, MonadIO m, MonadMask m)
  => PaymentClient m
  -> Payment
  -> m PaymentId
processPayment' client payment = recoverAll policy action
 where
  action RetryStatus {..} = do
    logInfo $ "[Checkout] - Processing payment #" <> T.pack (show rsIterNumber)
    PC.processPayment client payment

-- TODO: Run in the background once the number of retries has been reached
createOrder'
  :: (Logger m, MonadIO m, MonadMask m)
  => Orders m
  -> UserId
  -> PaymentId
  -> [CartItem]
  -> Money
  -> m OrderId
createOrder' orders uid pid items total = recoverAll policy action
 where
  action RetryStatus {..} = do
    logInfo $ "[Checkout] - Creating order #" <> T.pack (show rsIterNumber)
    SO.create orders uid pid items total

logWith :: Logger m => T.Text -> UserId -> m ()
logWith t UserId {..} = logInfo $ t <> UUID.toText unUserId

process'
  :: (Logger m, MonadIO m, MonadMask m)
  => PaymentClient m
  -> ShoppingCart m
  -> Orders m
  -> UserId
  -> Card
  -> m OrderId
process' pc sc so uid card = do
  logWith "[Checkout] - Retrieving shopping cart for " uid
  CartTotal {..} <- SC.get sc uid
  paymentId      <- processPayment' pc (payment cartTotal)
  orderId        <- createOrder' so uid paymentId cartItems cartTotal
  logWith "[Checkout] - Deleting shopping cart for " uid
  SC.delete sc uid
  pure orderId
  where payment t = Payment uid t card
