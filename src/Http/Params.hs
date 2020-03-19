{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Params where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Domain.Brand
import           Servant
import           Servant.API

(.+) = T.cons

capitalize :: Text -> Text
capitalize "" = ""
capitalize cs = toUpper (T.head cs) .+ T.map toLower (T.tail cs)

normalize :: Text -> Text
normalize = capitalize . (T.filter (/= '"'))

toBrandName :: BrandNameParam -> BrandName
toBrandName p = BrandName $ normalize (unBrandNameParam p)

-- TODO: Use refined
newtype BrandNameParam = BrandNameParam
  { unBrandNameParam :: Text
  } deriving (FromHttpApiData, Show)
