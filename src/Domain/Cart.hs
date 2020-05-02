{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}

module Domain.Cart where

import           Data.Aeson
import           Data.Map                       ( Map )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           Domain.Item
import           GHC.Generics                   ( Generic )

newtype CartId = CartId UUID deriving (Generic, ToRow, Show)
newtype Cart = Cart (Map ItemId Quantity) deriving (Generic, Show)
newtype Quantity = Quantity Int deriving (Generic, ToRow, Show)
newtype CartExpiration = CartExpiration Integer deriving (Generic, ToRow, Show)

data CartItem = CartItem
  { cartItem :: Item
  , cartQuantity :: Quantity
  } deriving (Generic, Show)

data CartTotal = CartTotal
  { cartItems :: [CartItem]
  , cartTotal :: Money
  } deriving (Generic, Show)

instance ToJSON Quantity where
  toJSON (Quantity q) = toJSON q

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
  toJSON (CartItem item quantity) =
    object ["item" .= item, "quantity" .= quantity]

instance ToJSON CartTotal where
  toJSON (CartTotal items total) =
    object ["items" .= items, "total" .= total]
