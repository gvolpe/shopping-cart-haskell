{-# LANGUAGE DataKinds, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Checkout where

import           Data.Aeson
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           GHC.Generics                   ( Generic )
import           Refined

type CardNamePred = Refined NonEmpty Text
type CardNumberPred = Refined (SizeEqualTo 16) Int
type CardExpirationPred = Refined (SizeEqualTo 4) Int
type CardCVVPred = Refined (SizeEqualTo 3) Int

newtype CardName = CardName {
  unCardName :: CardNamePred
} deriving (Generic, Show)
--} deriving (Generic, ToRow, Show)

newtype CardNumber = CardNumber {
  unCardNumber :: CardNumberPred
} deriving (Generic, Show)

newtype CardExpiration = CardExpiration {
  unCardExpiration :: CardExpirationPred
} deriving (Generic, Show)

newtype CardCVV = CardCVV {
  unCardCVV :: CardCVVPred
} deriving (Generic, Show)

data Card = Card
  { cardName :: CardName
  , cardNumber :: CardNumber
  , cardExpiration :: CardExpiration
  , cardCVV :: CardCVV
  } deriving (Generic, Show)

instance ToJSON Card where
  toJSON c = object
    [ "name" .= unrefine (unCardName $ cardName c)
    , "number" .= unrefine (unCardNumber $ cardNumber c)
    , "expiration" .= unrefine (unCardExpiration $ cardExpiration c)
    , "cvv" .= unrefine (unCardCVV $ cardCVV c)
    ]
