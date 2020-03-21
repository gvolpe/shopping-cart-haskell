module Programs.Checkout where

import           Data.UUID                      ( UUID )
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

mkCheckoutProgram
  :: Monad m => PaymentClient m -> ShoppingCart m -> Orders m -> m (Checkout m)
mkCheckoutProgram p s o = pure $ Checkout (checkout' p s o)

checkout'
  :: Monad m
  => PaymentClient m
  -> ShoppingCart m
  -> Orders m
  -> UserId
  -> Card
  -> m OrderId
checkout' pc sc so userId card = do
  ctl <- SC.get sc userId
  pid <- PC.processPayment pc payment
  oid <- SO.create so userId pid [ctl] (Money 123)
  SC.delete sc userId
  pure oid
 where
  card = undefined :: Card
  payment = Payment userId (Money 500) card
