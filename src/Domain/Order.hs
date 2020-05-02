{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}

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

newtype OrderId = OrderId UUID deriving (Eq, FromHttpApiData, Generic, Ord, Show)

instance ToRow OrderId

data Order = Order
  { orderId :: OrderId
  , orderPaymentId :: PaymentId
  , orderItems :: Map ItemId Quantity
  , orderTotal :: Money
  } deriving (Generic, Show)

instance ToJSON OrderId where
  toJSON (OrderId i) = toJSON i

instance ToJSON Order where
  toJSON (Order orderId paymentId items total) = object
    [ "order_id" .= orderId
    , "payment_id" .= paymentId
    , "items" .= items
    , "total" .= total
    ]
