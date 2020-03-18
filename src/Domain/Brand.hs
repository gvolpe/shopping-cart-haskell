{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Domain.Brand
    ( BrandId(..)
    , BrandName(..)
    , Brand(..)
    )
where

import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow )
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype BrandId = BrandId {
  unBrandId :: Text
} deriving (Generic, ToRow, Show)

newtype BrandName = BrandName {
  unBrandName :: Text
} deriving (Generic, ToRow, Show)

data Brand = Brand
  { brandId :: BrandId
  , brandName :: BrandName
  } deriving Show
