{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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

newtype CategoryId = CategoryId {
  unCategoryId :: UUID
} deriving (Generic, ToRow, Show)

newtype CategoryName = CategoryName {
  unCategoryName :: Text
} deriving (Generic, ToRow, Show)

data Category = Category
  { categoryId :: CategoryId
  , categoryName :: CategoryName
  } deriving (Generic, Show)

instance ToJSON Category where
  toJSON b = object
    [ "uuid" .= (unCategoryId $ categoryId b)
    , "name" .= (unCategoryName $ categoryName b)
    ]
