{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Http.Params where

import           Data.Bifunctor                 ( first )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Domain.Brand
import           Refined
import           Refined.Instances              ( )
import           Servant
import           Servant.API
import           Utils.Text                     ( normalize )

toBrandName :: BrandNameParam -> BrandName
toBrandName = BrandName . normalize . unrefine . unBrandNameParam

instance FromHttpApiData (Refined NonEmpty Text) where
  parseUrlPiece = first (T.pack . show) . refine

newtype BrandNameParam = BrandNameParam
  { unBrandNameParam :: Refined NonEmpty Text
  } deriving (FromHttpApiData, Show)
