{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Params where

import           Data.Bifunctor                 ( first )
import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Domain.Brand
import           Refined
import           Refined.Instances              ( )
import           Servant
import           Servant.API

capitalize :: Text -> Text
capitalize "" = ""
capitalize cs =
  let h = toUpper (T.head cs)
      t = T.map toLower (T.tail cs)
  in  T.cons h t

normalize :: Text -> Text
normalize = capitalize . T.filter (/= '"')

toBrandName :: BrandNameParam -> BrandName
toBrandName = BrandName . normalize . unrefine . unBrandNameParam

instance FromHttpApiData (Refined NonEmpty Text) where
  parseUrlPiece = first (T.pack . show) . refine

newtype BrandNameParam = BrandNameParam
  { unBrandNameParam :: Refined NonEmpty Text
  } deriving (FromHttpApiData, Show)
