{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Domain.Order where

import           Control.Monad.Catch
import           Data.Aeson
import           Data.Map
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple
import           Domain.Cart
import           Domain.Item
import           Domain.Payment
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData )

newtype OrderId = OrderId UUID
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype FromHttpApiData

instance ToRow OrderId

data Order = Order
  { orderId :: OrderId
  , orderPaymentId :: PaymentId
  , orderItems :: Map ItemId Quantity
  , orderTotal :: Money
  } deriving (Generic, Show)

data EmptyCartError = EmptyCartError
  deriving stock Show
  deriving anyclass Exception

instance ToJSON OrderId where
  toJSON (OrderId i) = toJSON i

instance ToJSON Order where
  toJSON (Order oid paymentId _items _total) = object
    [ "order_id" .= oid
    , "payment_id" .= paymentId
    , "items" .= _items
    , "total" .= _total
    ]
