{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Domain.Order where

import           Data.Aeson
import           Data.Map
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple
import           Domain.Cart
import           Domain.Item
import           Domain.Payment
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData )

newtype OrderId = OrderId {
  unOrderId :: UUID
} deriving (Eq, FromHttpApiData, Generic, Ord, Show)

instance ToRow OrderId

data Order = Order
  { orderId :: OrderId
  , orderPaymentId :: PaymentId
  , orderItems :: Map ItemId Quantity
  , orderTotal :: Money
  } deriving (Generic, Show)

instance ToJSON OrderId where
  toJSON i = toJSON $ unOrderId i

instance ToJSON Order where
  toJSON Order {..} = object
    [ "order_id" .= toJSON orderId
    , "payment_id" .= unPaymentId orderPaymentId
    , "items" .= toJSON orderItems
    , "total" .= unMoney orderTotal
    ]
