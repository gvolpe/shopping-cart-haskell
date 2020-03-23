{-# LANGUAGE RecordWildCards #-}

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
import           Services.Orders                ( Orders )
import qualified Services.Orders               as SO
import           Services.ShoppingCart          ( ShoppingCart )
import qualified Services.ShoppingCart         as SC

data Checkout m = Checkout
  { checkout :: UserId -> Card -> m OrderId
  }

mkCheckout
  :: Monad m => PaymentClient m -> ShoppingCart m -> Orders m -> m (Checkout m)
mkCheckout p s o = pure $ Checkout (checkout' p s o)

checkout'
  :: Monad m
  => PaymentClient m
  -> ShoppingCart m
  -> Orders m
  -> UserId
  -> Card
  -> m OrderId
checkout' pc sc so userId card = do
  CartTotal {..} <- SC.get sc userId
  paymentId      <- PC.processPayment pc (payment cartTotal)
  orderId        <- SO.create so userId paymentId cartItems cartTotal
  SC.delete sc userId
  pure orderId
  where payment t = Payment userId t card
