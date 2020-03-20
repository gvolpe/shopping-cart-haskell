module Services.Orders where

import Domain.Cart
import Domain.Order
import Domain.Payment
import Domain.User

data Orders m = Orders
  { get :: UserId -> OrderId -> m (Maybe Order)
  , findBy :: UserId -> m [Order]
  , create :: UserId -> PaymentId -> [CartItem] -> Money -> m OrderId
  }
