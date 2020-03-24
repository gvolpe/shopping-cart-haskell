{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Routes.Brands where

import           Control.Monad.IO.Class         ( liftIO )
import           Domain.Brand
import           Effects.Logger
import           Http.Params
import           Http.Routes.Version
import           Servant
import           Services.Brands                ( Brands )
import qualified Services.Brands               as SB

type BrandsAPI =
  ApiVersion :> "brands" :> Get '[JSON] [Brand]

brandsServer :: Brands IO -> Server BrandsAPI
brandsServer = findBrands

findBrands :: Brands IO -> Handler [Brand]
findBrands b = do
  logInfo "[Brands] - Find all"
  liftIO $ SB.findAll b
