{-# LANGUAGE OverloadedStrings #-}

module Http.Handler where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Monoid                    ( (<>) )
import           Domain.Brand
import           Domain.Item
import           Http.Params
import           Logger
import           Servant
import           Services.Brands                ( Brands )
import qualified Services.Brands               as SB
import           Services.Items                 ( Items )
import qualified Services.Items                as SI

findBrands :: Brands IO -> Handler [Brand]
findBrands b = do
  logInfo "[Brands] - Find all"
  liftIO $ SB.findAll b

findItems :: Items IO -> Maybe BrandNameParam -> Handler [Item]
findItems i (Just bn) = do
  logInfo $ "[Items] - Find by brand: " <> unBrandName brand
  liftIO $ SI.findBy i brand
  where brand = toBrandName bn
findItems i Nothing = do
  logInfo "[Items] - Find all"
  liftIO $ SI.findAll i
