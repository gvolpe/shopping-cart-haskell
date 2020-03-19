{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Services.Brands
  ( Brands(..)
  , mkLiveBrands
  )
where

import           Control.Monad.Catch
import           Data.Functor                   ( void )
import           Database.PostgreSQL.Simple
import           Data.UUID                      ( UUID
                                                , toText
                                                )
import           Data.UUID.V4                   ( nextRandom )
import           Data.Text
import           Domain.Brand
import           GHC.Generics                   ( Generic )

data Brands m = Brands
  { findAllBrands :: m [Brand]
  , createBrand :: BrandName -> m ()
  }

mkLiveBrands :: Connection -> IO (Brands IO)
mkLiveBrands c = pure $ Brands
  { findAllBrands = (fmap . fmap) toDomain (findAll' c)
  , createBrand   = create' c
  }

data BrandDTO = BrandDTO
  { _brandId :: UUID
  , _brandName :: Text
  } deriving (Generic, FromRow, ToRow, Show)

toDomain :: BrandDTO -> Brand
toDomain dto =
  Brand (BrandId . toText $ _brandId dto) (BrandName $ _brandName dto)

findAll' :: Connection -> IO [BrandDTO]
findAll' = flip query_ "SELECT * FROM brands"

create' :: Connection -> BrandName -> IO ()
create' c b = do
  uuid <- nextRandom
  void $ executeMany c
                     "INSERT INTO brands (uuid, name) VALUES (?, ?)"
                     [(uuid, unBrandName b)]
