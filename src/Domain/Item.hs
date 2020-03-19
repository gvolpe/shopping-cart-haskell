{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Domain.Item
  ( ItemId(..)
  , ItemName(..)
  , ItemDescription(..)
  , Item(..)
  , CreateItem(..)
  , UpdateItem(..)
  , Category(..)
  , Money(..)
  )
where

import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text )
import           Domain.Brand                   ( Brand )
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

-- TODO: This should be defined in Domain.Category
newtype Category = Category {
 unCategory :: Text
} deriving (Generic, ToRow, Show)

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
