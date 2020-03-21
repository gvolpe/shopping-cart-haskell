{-# LANGUAGE DataKinds, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Checkout
  ( Card(..)
  , CardName(..)
  , CardNumber(..)
  , CardExpiration(..)
  , CardCVV(..)
  )
where

import           Data.Aeson
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           GHC.Generics                   ( Generic )
import           Refined

newtype CardName = CardName {
  unCardName :: Refined NonEmpty Text
} deriving (Generic, Show)
--} deriving (Generic, ToRow, Show)

newtype CardNumber = CardNumber {
  unCardNumber :: Refined (SizeEqualTo 16) Int
} deriving (Generic, Show)

newtype CardExpiration = CardExpiration {
  unCardExpiration :: Refined (SizeEqualTo 4) Int
} deriving (Generic, Show)

newtype CardCVV = CardCVV {
  unCardCVV :: Refined (SizeEqualTo 3) Int
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
