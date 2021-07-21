{-# LANGUAGE DataKinds, OverloadedLabels, OverloadedStrings, TypeOperators #-}

module Http.Routes.Items where

import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Generics.Labels           ( )
import           Domain.Brand
import           Domain.Item
import           Effects.Logger
import           Http.Params
import           Http.Routes.Version
import           Servant
import           Services.Items                 ( Items )

type ItemsAPI =
  ApiVersion :> "items" :> QueryParam "brand" BrandNameParam :> Get '[JSON] [Item]

itemsServer :: Items IO -> Server ItemsAPI
itemsServer = findItems

findItems :: Items IO -> Maybe BrandNameParam -> Handler [Item]
findItems items (Just bn) = do
  logInfo $ "[Items] - Find by brand: " <> b
  brand & items ^. #findBy & liftIO
  where brand@(BrandName b) = toBrandName bn
findItems items Nothing = do
  logInfo "[Items] - Find all"
  items ^. #findAll & liftIO
