{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Domain.Cart where

import           Data.Aeson
import           Data.Aeson.Types               ( Parser )
import           Data.Bifunctor                 ( bimap )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
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

instance FromJSON Quantity where
  parseJSON v = Quantity <$> parseJSON v

instance FromJSON Cart where
  parseJSON = withObject "Cart json" $ \o -> do
    x <- o .: "items" :: Parser (Map UUID Int)
    return $ Cart (M.fromList $ items x)
    where items x = bimap ItemId Quantity <$> M.toList x

instance ToJSON CartItem where
  toJSON CartItem {..} =
    object ["item" .= cartItem, "quantity" .= unQuantity cartQuantity]

instance ToJSON CartTotal where
  toJSON CartTotal {..} =
    object ["items" .= toJSON cartItems, "total" .= toJSON (unMoney cartTotal)]
