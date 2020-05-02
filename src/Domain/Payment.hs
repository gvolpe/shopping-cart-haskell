{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

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

newtype PaymentId = PaymentId UUID deriving (Eq, Generic, ToRow, Show)

data Payment = Payment
  { paymentUserId :: UserId
  , paymentTotal :: Money
  , paymentCard :: Card
  } deriving (Generic, Show)

instance FromJSON PaymentId where
  parseJSON (Object v) = PaymentId <$> v .: "paymentId"

instance ToJSON PaymentId where
  toJSON (PaymentId i) = toJSON i

instance ToJSON Payment where
  toJSON (Payment uid total card) = object
    [ "user_id" .= uid
    , "total" .= total
    , "card" .= card
    ]
