{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}

module Domain.Brand
  ( BrandId(..)
  , BrandName(..)
  , Brand(..)
  )
where

import           Data.Aeson
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           GHC.Generics                   ( Generic )

newtype BrandId = BrandId UUID deriving (Generic, ToRow, Show)
newtype BrandName = BrandName Text deriving (Generic, ToRow, Show)

data Brand = Brand
  { brandId :: BrandId
  , brandName :: BrandName
  } deriving (Generic, Show)

instance FromJSON Brand where
  parseJSON = withObject "Brand json" $ \o -> do
    i <- o .: "uuid"
    n <- o .: "name"
    return $ Brand (BrandId i) (BrandName n)

instance ToJSON Brand where
  toJSON (Brand (BrandId bid) (BrandName bname)) =
    object ["uuid" .= bid, "name" .= bname]
