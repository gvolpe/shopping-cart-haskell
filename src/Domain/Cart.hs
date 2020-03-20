module Domain.Cart where

import           Data.UUID                      ( UUID )

type Cart = [String]
type CartItem = String
type CartTotal = String
type Quantity = Int
type Money = Double
