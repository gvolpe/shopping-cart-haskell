{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
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
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text )
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
} deriving (Generic, ToRow, Show)

newtype ItemName = ItemName {
 unItemName :: Text
} deriving (Generic, ToRow, Show)

newtype ItemDescription = ItemDescription {
 unItemDescription :: Text
} deriving (Generic, ToRow, Show)

newtype Money = Money {
 unMoney :: Double
} deriving (Generic, Show)

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

instance ToJSON Item where
  toJSON i = object
    [ "uuid" .= unItemId (itemId i)
    , "name" .= unItemName (itemName i)
    , "description" .= unItemDescription (itemDescription i)
    , "brand" .= toJSON (itemBrand i)
    , "category" .= toJSON (itemCategory i)]
