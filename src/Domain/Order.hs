{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Domain.Order where

import           Data.Aeson
import           Data.Map
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple
import           Domain.Cart                    ( Quantity )
import           Domain.Item                    ( ItemId
                                                , Money
                                                )
import           Domain.Payment                 ( PaymentId )
import           GHC.Generics                   ( Generic )

newtype OrderId = OrderId {
  unOrderId :: UUID
} deriving (Eq, Generic, Ord, Show, ToRow)

data Order = Order
  { orderId :: OrderId
  , orderPaymentId :: PaymentId
  , orderItems :: Map ItemId Quantity
  , orderTotal :: Money
  } deriving (Generic, Show)
