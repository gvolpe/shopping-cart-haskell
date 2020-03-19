{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Services.Items where

import           Data.Functor                   ( void )
--import           Data.Ratio                     ( Ratio )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID
                                                , toText
                                                )
import           Data.UUID.V4                   ( nextRandom )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Domain.Brand
import           Domain.Item
import           GHC.Generics                   ( Generic )
--import NumHask.Data.Rational (fromRatio)
import           GHC.Real                       ( Ratio )

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
  , _itemPrice :: Ratio Integer
  , _brandId :: UUID
  , _categoryId :: UUID
  } deriving (Generic, FromRow, Show)

toDomain :: ItemDTO -> Item
toDomain dto = Item
  { itemId          = ItemId $ _itemId dto
  , itemName        = ItemName $ _itemName dto
  , itemDescription = ItemDescription $ _itemDescription dto
  , itemPrice       = Money $ _itemPrice dto
  , itemBrand       = Brand (BrandId . toText $ _brandId dto) (BrandName "foo")
  , itemCategory    = Category (toText $ _categoryId dto)
  }

findAll' :: Connection -> IO [ItemDTO]
findAll' = flip query_ "SELECT * FROM items"

findBy' :: Connection -> BrandName -> IO [ItemDTO]
findBy' = flip query "SELECT * FROM items WHERE name=?"

findById' :: Connection -> BrandId -> IO (Maybe Item)
findById' _ _ = pure Nothing

create' :: Connection -> CreateItem -> IO ()
create' _ _ = pure ()

update' :: Connection -> UpdateItem -> IO ()
update' _ _ = pure ()
