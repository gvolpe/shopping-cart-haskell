module Services.ShoppingCart where

import Domain.Cart
import Domain.Item
import Domain.User

data ShoppingCart m = ShoppingCart
  { add :: UserId -> ItemId -> Quantity -> m ()
  , get :: UserId -> m CartTotal
  , delete :: UserId -> m ()
  , removeItem :: UserId -> ItemId -> m ()
  , update :: UserId -> Cart -> m ()
  }
