{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Services.Brands
  ( Brands(..)
  , mkLiveBrands
  )
where

import           Data.Functor                   ( void )
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )
import           Data.Text
import           Database.PostgreSQL.Simple
import           Domain.Brand
import           GHC.Generics                   ( Generic )

data Brands m = Brands
  { findAll :: m [Brand]
  , create :: BrandName -> m ()
  }

mkLiveBrands :: Connection -> IO (Brands IO)
mkLiveBrands c = pure $ Brands { findAll = (fmap . fmap) toDomain (findAll' c)
                               , create  = create' c
                               }

data BrandDTO = BrandDTO
  { _brandId :: UUID
  , _brandName :: Text
  } deriving (Generic, FromRow, ToRow, Show)

toDomain :: BrandDTO -> Brand
toDomain dto =
  Brand (BrandId $ _brandId dto) (BrandName $ _brandName dto)

findAll' :: Connection -> IO [BrandDTO]
findAll' = flip query_ "SELECT * FROM brands"

create' :: Connection -> BrandName -> IO ()
create' c b = do
  uuid <- nextRandom
  void $ executeMany c
                     "INSERT INTO brands (uuid, name) VALUES (?, ?)"
                     [(uuid, unBrandName b)]
