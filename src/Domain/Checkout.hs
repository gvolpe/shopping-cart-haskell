{-# LANGUAGE DataKinds, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Domain.Checkout where

import           Data.Aeson
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Refined

type CardNamePred = Refined NonEmpty Text
type CardNumberPred = Refined (SizeEqualTo 16) Int
type CardExpirationPred = Refined (SizeEqualTo 4) Int
type CardCVVPred = Refined (SizeEqualTo 3) Int

newtype CardName = CardName {
  unCardName :: CardNamePred
} deriving (Generic, Show)

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
  toJSON Card {..} = object
    [ "name" .= unrefine (unCardName cardName)
    , "number" .= unrefine (unCardNumber cardNumber)
    , "expiration" .= unrefine (unCardExpiration cardExpiration)
    , "cvv" .= unrefine (unCardCVV cardCVV)
    ]
