{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Services.Items
  ( Items(..)
  , mkItems
  )
where

import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple
import           Domain.Brand
import           Domain.Category
import           Domain.Item
import           GHC.Generics                   ( Generic )
import           Text.RawString.QQ

data Items m = Items
  { findAll :: m [Item]
  , findBy :: BrandName -> m [Item]
  , findById :: ItemId -> m (Maybe Item)
  , create :: CreateItem -> m ()
  , update :: UpdateItem -> m ()
  } deriving Generic

mkItems :: Connection -> Items IO
mkItems c = Items { findAll  = (fmap . fmap) toDomain (findAll' c)
                  , findBy   = (fmap . fmap) toDomain . findBy' c
                  , findById = (fmap . fmap) toDomain . findById' c
                  , create   = create' c
                  , update   = update' c
                  }

data ItemDTO = ItemDTO
  { _itemId :: UUID
  , _itemName :: Text
  , _itemDescription :: Text
  , _itemPrice :: Rational
  , _itemBrandId :: UUID
  , _itemBrandName :: Text
  , _itemCategoryId :: UUID
  , _itemCategoryName :: Text
  } deriving (Generic, FromRow, Show)

toDomain :: ItemDTO -> Item
toDomain ItemDTO {..} = Item
  { itemId          = ItemId _itemId
  , itemName        = ItemName _itemName
  , itemDescription = ItemDescription _itemDescription
  , itemPrice       = Money $ fromRational _itemPrice
  , itemBrand       = Brand (BrandId _itemBrandId) (BrandName _itemBrandName)
  , itemCategory    = Category (CategoryId _itemCategoryId)
                               (CategoryName _itemCategoryName)
  }

selectAllQuery :: Query
selectAllQuery =
  [r|SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
     FROM items AS i
     INNER JOIN brands AS b ON i.brand_id = b.uuid
     INNER JOIN categories AS c ON i.category_id = c.uuid|]

findAll' :: Connection -> IO [ItemDTO]
findAll' = flip query_ selectAllQuery

selectByBrandQuery :: Query
selectByBrandQuery =
  [r|SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
     FROM items AS i
     INNER JOIN brands AS b ON i.brand_id = b.uuid
     INNER JOIN categories AS c ON i.category_id = c.uuid
     WHERE b.name LIKE ?|]

findBy' :: Connection -> BrandName -> IO [ItemDTO]
findBy' = flip query selectByBrandQuery

selectByItemIdQuery :: Query
selectByItemIdQuery =
  [r|SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
     FROM items AS i
     INNER JOIN brands AS b ON i.brand_id = b.uuid
     INNER JOIN categories AS c ON i.category_id = c.uuid
     WHERE i.uuid = ?|]

findById' :: Connection -> ItemId -> IO (Maybe ItemDTO)
findById' c i = listToMaybe <$> query c selectByItemIdQuery i

create' :: Connection -> CreateItem -> IO ()
create' _ _ = pure ()

update' :: Connection -> UpdateItem -> IO ()
update' _ _ = pure ()
