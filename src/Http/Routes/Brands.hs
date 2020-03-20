{-# LANGUAGE DataKinds, TypeOperators #-}

module Http.Routes.Brands where

import           Domain.Brand
import qualified Http.Handler                   as Handler
import           Http.Params
import           Http.Routes.Version
import           Servant
import           Services.Brands

type BrandsAPI =
  ApiVersion :> "brands" :> Get '[JSON] [Brand]

brandsServer :: Brands IO -> Server BrandsAPI
brandsServer = Handler.findBrands
