{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Cart where

import           Data.Aeson
import           Data.Map                       ( Map )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           Domain.Item
import           GHC.Generics                   ( Generic )

newtype CartId = CartId {
  unCartId :: UUID
} deriving (Generic, ToRow, Show)

newtype Cart = Cart {
  unCart :: Map ItemId Quantity
} deriving (Generic, Show)

data CartItem = CartItem
  { cartItem :: Item
  , cartQuantity :: Quantity
  } deriving (Generic, Show)

data CartTotal = CartTotal
  { cartItems :: [CartItem]
  , cartTotal :: Money
  } deriving (Generic, Show)

newtype Quantity = Quantity {
  unQuantity :: Int
} deriving (Generic, ToRow, Show)

newtype CartExpiration = CartExpiration {
  unCartExpiration :: Integer
} deriving (Generic, ToRow, Show)

instance ToJSON CartItem where
  toJSON i = object
    [ "item" .= toJSON (cartItem i)
    , "quantity" .= toJSON (unQuantity $ cartQuantity i)
    ]

instance ToJSON CartTotal where
  toJSON t = object
    ["items" .= toJSON (cartItems t), "total" .= toJSON (unMoney $ cartTotal t)]
