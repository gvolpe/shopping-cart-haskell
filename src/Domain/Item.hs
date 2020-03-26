{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingVia #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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
import           Database.PostgreSQL.Simple
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

instance ToJSON ItemId where
  toJSON i = toJSON $ unItemId i

instance FromJSON Money where
  parseJSON v = Money <$> parseJSON v

instance ToJSON Money where
  toJSON m = toJSON $ unMoney m

instance FromJSONKey ItemId
instance ToJSONKey ItemId

instance FromJSON Item where
  parseJSON = withObject "Item json" $ \o -> do
    i  <- o .: "uuid"
    n  <- o .: "name"
    d  <- o .: "description"
    p  <- o .: "price"
    b  <- o .: "brand"
    c  <- o .: "category"
    return $ Item i (ItemName n) (ItemDescription d) p b c

instance ToJSON Item where
  toJSON Item {..} = object
    [ "uuid" .= itemId
    , "name" .= unItemName itemName
    , "description" .= unItemDescription itemDescription
    , "price" .= itemPrice
    , "brand" .= itemBrand
    , "category" .= itemCategory
    ]
