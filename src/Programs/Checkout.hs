{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Programs.Checkout
  ( Checkout(..)
  , mkCheckout
  )
where

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
  :: (Logger m, Monad m)
  => PaymentClient m
  -> ShoppingCart m
  -> Orders m
  -> m (Checkout m)
mkCheckout p s o = pure $ Checkout (process' p s o)

-- TODO: Move logging to retry functions
process'
  :: (Logger m, Monad m)
  => PaymentClient m
  -> ShoppingCart m
  -> Orders m
  -> UserId
  -> Card
  -> m OrderId
process' pc sc so userId card = do
  logInfo "[Checkout] - Retrieving shopping cart for user"
  CartTotal {..} <- SC.get sc userId
  logInfo "[Checkout] - Processing payment"
  paymentId <- PC.processPayment pc (payment cartTotal)
  logInfo "[Checkout] - Creating order"
  orderId <- SO.create so userId paymentId cartItems cartTotal
  logInfo "[Checkout] - Deleting shopping cart from cache"
  SC.delete sc userId
  pure orderId
  where payment t = Payment userId t card
