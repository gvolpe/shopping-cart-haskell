{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

module Http.Routes.Brands where

import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Generics.Labels           ( )
import           Domain.Brand
import           Effects.Logger
import           Http.Routes.Version
import           Servant
import           Services.Brands                ( Brands )

type BrandsAPI =
  ApiVersion :> "brands" :> Get '[JSON] [Brand]

brandsServer :: Brands IO -> Server BrandsAPI
brandsServer = findBrands

findBrands :: Brands IO -> Handler [Brand]
findBrands brands = do
  logInfo "[Brands] - Find all"
  brands ^. #findAll & liftIO
