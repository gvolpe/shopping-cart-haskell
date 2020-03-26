{-# LANGUAGE DeriveAnyClass, DeriveGeneric, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Domain.Payment where

import           Data.Aeson
import           Data.UUID                      ( UUID )
import qualified Data.Text                     as T
import           Domain.Checkout
import           Domain.Item
import           Domain.User
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           GHC.Generics                   ( Generic )

newtype PaymentId = PaymentId {
  unPaymentId :: UUID
} deriving (Eq, Generic, ToRow, Show)

data Payment = Payment
  { paymentUserId :: UserId
  , paymentTotal :: Money
  , paymentCard :: Card
  } deriving (Generic, Show)

instance FromJSON PaymentId where
  parseJSON (Object v) = PaymentId <$> v .: "paymentId"

instance ToJSON PaymentId where
  toJSON i = toJSON $ unPaymentId i

instance ToJSON Payment where
  toJSON Payment {..} = object
    [ "user_id" .= paymentUserId
    , "total" .= paymentTotal
    , "card" .= paymentCard
    ]
