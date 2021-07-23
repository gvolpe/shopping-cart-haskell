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
import           Database.PostgreSQL.Resilient  ( ResilientConnection(..) )
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

mkItems :: ResilientConnection IO -> Items IO
mkItems p = Items { findAll  = (fmap . fmap) toDomain (findAll' p)
                  , findBy   = (fmap . fmap) toDomain . findBy' p
                  , findById = (fmap . fmap) toDomain . findById' p
                  , create   = create' p
                  , update   = update' p
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

findAll' :: ResilientConnection IO -> IO [ItemDTO]
findAll' pool = do
  conn <- getConnection pool
  query_ conn selectAllQuery

selectByBrandQuery :: Query
selectByBrandQuery =
  [r|SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
     FROM items AS i
     INNER JOIN brands AS b ON i.brand_id = b.uuid
     INNER JOIN categories AS c ON i.category_id = c.uuid
     WHERE b.name LIKE ?|]

findBy' :: ResilientConnection IO -> BrandName -> IO [ItemDTO]
findBy' pool brand = do
  conn <- getConnection pool
  query conn selectByBrandQuery brand

selectByItemIdQuery :: Query
selectByItemIdQuery =
  [r|SELECT i.uuid, i.name, i.description, i.price, b.uuid, b.name, c.uuid, c.name
     FROM items AS i
     INNER JOIN brands AS b ON i.brand_id = b.uuid
     INNER JOIN categories AS c ON i.category_id = c.uuid
     WHERE i.uuid = ?|]

findById' :: ResilientConnection IO -> ItemId -> IO (Maybe ItemDTO)
findById' pool i = do
  conn <- getConnection pool
  listToMaybe <$> query conn selectByItemIdQuery i

create' :: ResilientConnection IO -> CreateItem -> IO ()
create' _ _ = pure ()

update' :: ResilientConnection IO -> UpdateItem -> IO ()
update' _ _ = pure ()
