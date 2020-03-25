{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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

instance ToJSON Quantity where
  toJSON q = toJSON (unQuantity q)

instance FromJSON Quantity where
  parseJSON v = Quantity <$> parseJSON v

instance FromJSON Cart where
  parseJSON = withObject "Cart json" $ \o -> do
    i <- o .: "items"
    return $ Cart i

instance FromJSON CartItem where
  parseJSON = withObject "CartItem json" $ \o -> do
    i <- o .: "item"
    q <- o .: "quantity"
    return $ CartItem i q

instance ToJSON CartItem where
  toJSON CartItem {..} =
    object ["item" .= cartItem, "quantity" .= cartQuantity]

instance ToJSON CartTotal where
  toJSON CartTotal {..} =
    object ["items" .= cartItems, "total" .= cartTotal]
