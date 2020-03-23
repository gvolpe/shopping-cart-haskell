module Services where

import           Http.Clients.Payments
import           Services.Brands
import           Services.Items
import           Services.Orders
import           Services.ShoppingCart

data Services m = Services
  { brands :: Brands m
  , cart :: ShoppingCart m
  , items :: Items m
  , orders :: Orders m
  , payments :: PaymentClient m
  }
