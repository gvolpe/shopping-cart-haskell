module Services where

import           Http.Clients.Payments
import           Services.Brands
import           Services.Items
import           Services.ShoppingCart

data Services m = Services
  { brands :: Brands m
  , cart :: ShoppingCart m
  , items :: Items m
  , payments :: PaymentClient m
  }
