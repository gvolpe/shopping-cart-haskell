module Services where

import Services.Brands
import Services.Items

data Services m = Services
  { brands :: Brands m
  , items :: Items m
  }
