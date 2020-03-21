module Services where

import           Http.Clients.Payments
import           Services.Brands
import           Services.Items

data Services m = Services
  { brands :: Brands m
  , items :: Items m
  , payments :: PaymentClient m
  }
