{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Domain.Payment where

import           Data.UUID                      ( UUID )
import           Domain.Checkout                ( Card )
import           Domain.Item                    ( Money )
import           Domain.User                    ( UserId )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           GHC.Generics                   ( Generic )

newtype PaymentId = PaymentId {
  unPaymentId :: UUID
} deriving (Generic, ToRow, Show)

data Payment = Payment
  { paymentUserId :: UserId
  , paymentTotal :: Money
  , paymentCard :: Card
  } deriving (Generic, Show)
