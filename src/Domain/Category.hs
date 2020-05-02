{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}

module Domain.Category
  ( CategoryId(..)
  , CategoryName(..)
  , Category(..)
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

newtype CategoryId = CategoryId UUID deriving (Generic, ToRow, Show)
newtype CategoryName = CategoryName Text deriving (Generic, ToRow, Show)

data Category = Category
  { categoryId :: CategoryId
  , categoryName :: CategoryName
  } deriving (Generic, Show)

instance FromJSON Category where
  parseJSON = withObject "Brand json" $ \o -> do
    i <- o .: "uuid"
    n <- o .: "name"
    return $ Category (CategoryId i) (CategoryName n)

instance ToJSON Category where
  toJSON (Category (CategoryId cid) (CategoryName cname)) = object
    ["uuid" .= cid, "name" .= cname]
