{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Services.Brands
  ( Brands(..)
  , mkBrands
  )
where

import           Data.Functor                   ( void )
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple
import           Domain.Brand
import           GHC.Generics                   ( Generic )

data Brands m = Brands
  { findAll :: m [Brand]
  , create :: BrandName -> m ()
  }

mkBrands :: Connection -> IO (Brands IO)
mkBrands c = pure $ Brands { findAll = (fmap . fmap) toDomain (findAll' c)
                           , create  = create' c
                           }

data BrandDTO = BrandDTO
  { _brandId :: UUID
  , _brandName :: Text
  } deriving (Generic, FromRow, ToRow, Show)

toDomain :: BrandDTO -> Brand
toDomain BrandDTO {..} = Brand (BrandId _brandId) (BrandName _brandName)

findAll' :: Connection -> IO [BrandDTO]
findAll' = flip query_ "SELECT * FROM brands"

create' :: Connection -> BrandName -> IO ()
create' c (BrandName bname) = do
  uuid <- nextRandom
  void $ executeMany c
                     "INSERT INTO brands (uuid, name) VALUES (?, ?)"
                     [(uuid, bname)]
