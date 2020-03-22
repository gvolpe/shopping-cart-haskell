{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Item
  ( ItemId(..)
  , ItemName(..)
  , ItemDescription(..)
  , Item(..)
  , CreateItem(..)
  , UpdateItem(..)
  , Money(..)
  )
where

import           Data.Aeson
import           Data.Monoid                    ( Sum(..) )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           Domain.Brand
import           Domain.Category
import           GHC.Generics                   ( Generic )
import           GHC.Real                       ( Ratio )

newtype ItemId = ItemId {
 unItemId :: UUID
} deriving (Eq, Generic, Ord, Show, ToRow)

newtype ItemName = ItemName {
 unItemName :: Text
} deriving (Generic, ToRow, Show)

newtype ItemDescription = ItemDescription {
 unItemDescription :: Text
} deriving (Generic, ToRow, Show)

newtype Money = Money {
 unMoney :: Double
} deriving stock (Generic, Show)
  deriving Num via (Sum Double)
  deriving Semigroup via (Sum Double)
  deriving Monoid via (Sum Double)

data Item = Item
  { itemId :: ItemId
  , itemName :: ItemName
  , itemDescription :: ItemDescription
  , itemPrice :: Money
  , itemBrand :: Brand
  , itemCategory :: Category
  } deriving (Generic, Show)

type CreateItem = Item
type UpdateItem = Item

instance FromJSON ItemId where
  parseJSON v = ItemId <$> parseJSON v

instance FromJSONKey ItemId where

instance ToJSON Item where
  toJSON i = object
    [ "uuid" .= unItemId (itemId i)
    , "name" .= unItemName (itemName i)
    , "description" .= unItemDescription (itemDescription i)
    , "brand" .= toJSON (itemBrand i)
    , "category" .= toJSON (itemCategory i)
    ]
