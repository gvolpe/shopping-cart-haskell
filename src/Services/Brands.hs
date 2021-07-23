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
import           Database.PostgreSQL.Resilient  ( ResilientConnection(..) )
import           Database.PostgreSQL.Simple
import           Domain.Brand
import           GHC.Generics                   ( Generic )

data Brands m = Brands
  { findAll :: m [Brand]
  , create :: BrandName -> m ()
  } deriving Generic

mkBrands :: ResilientConnection IO -> Brands IO
mkBrands p =
  Brands { findAll = (fmap . fmap) toDomain (findAll' p), create = create' p }

data BrandDTO = BrandDTO
  { _brandId :: UUID
  , _brandName :: Text
  } deriving (Generic, FromRow, ToRow, Show)

toDomain :: BrandDTO -> Brand
toDomain BrandDTO {..} = Brand (BrandId _brandId) (BrandName _brandName)

findAll' :: ResilientConnection IO -> IO [BrandDTO]
findAll' pool = do
  conn <- getConnection pool
  query_ conn "SELECT * FROM brands"

create' :: ResilientConnection IO -> BrandName -> IO ()
create' pool (BrandName bname) = do
  uuid <- nextRandom
  conn <- getConnection pool
  void $ executeMany conn
                     "INSERT INTO brands (uuid, name) VALUES (?, ?)"
                     [(uuid, bname)]
