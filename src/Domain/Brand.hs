{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Brand
  ( BrandId(..)
  , BrandName(..)
  , Brand(..)
  )
where

import           Data.Aeson
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           GHC.Generics                   ( Generic )

newtype BrandId = BrandId {
  unBrandId :: UUID
} deriving (Generic, ToRow, Show)

newtype BrandName = BrandName {
  unBrandName :: Text
} deriving (Generic, ToRow, Show)

data Brand = Brand
  { brandId :: BrandId
  , brandName :: BrandName
  } deriving (Generic, Show)

instance ToJSON Brand where
  toJSON b = object
    ["uuid" .= unBrandId (brandId b), "name" .= unBrandName (brandName b)]
