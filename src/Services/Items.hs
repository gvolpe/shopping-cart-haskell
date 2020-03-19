{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Services.Items where

import           Data.Functor                   ( void )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID
                                                , toText
                                                )
import           Data.UUID.V4                   ( nextRandom )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Domain.Brand
import           Domain.Category
import           Domain.Item
import           GHC.Generics                   ( Generic )
import           GHC.Real                       ( Rational )
import           Text.RawString.QQ

data Items m = Items
  { findAll :: m [Item]
  , findBy :: BrandName -> m [Item]
  , findById :: BrandId -> m (Maybe Item)
  , create :: CreateItem -> m ()
  , update :: UpdateItem -> m ()
  }

mkLiveItems :: Connection -> IO (Items IO)
mkLiveItems c = pure $ Items { findAll  = (fmap . fmap) toDomain (findAll' c)
                             , findBy   = (fmap . fmap) toDomain . findBy' c
                             , findById = findById' c
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
toDomain dto = Item
  { itemId          = ItemId $ _itemId dto
  , itemName        = ItemName $ _itemName dto
  , itemDescription = ItemDescription $ _itemDescription dto
  , itemPrice       = Money . fromRational $ _itemPrice dto
  , itemBrand       = Brand (BrandId $ _itemBrandId dto)
                            (BrandName $ _itemBrandName dto)
  , itemCategory    = Category (CategoryId $ _itemCategoryId dto)
                               (CategoryName $ _itemCategoryName dto)
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

findById' :: Connection -> BrandId -> IO (Maybe Item)
findById' _ _ = pure Nothing

create' :: Connection -> CreateItem -> IO ()
create' _ _ = pure ()

update' :: Connection -> UpdateItem -> IO ()
update' _ _ = pure ()
