{-# LANGUAGE DerivingStrategies, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Http.Params where

import           Data.Text                      ( Text )
import           Domain.Brand
import           Orphan                         ( )
import           Refined
import           Servant
import           Utils.Text                     ( normalize )

toBrandName :: BrandNameParam -> BrandName
toBrandName (BrandNameParam p) = BrandName . normalize $ unrefine p

newtype BrandNameParam = BrandNameParam (Refined NonEmpty Text)
  deriving stock Show
  deriving newtype FromHttpApiData
