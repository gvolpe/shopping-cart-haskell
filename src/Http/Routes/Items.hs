{-# LANGUAGE DataKinds, TypeOperators #-}

module Http.Routes.Items where

import           Domain.Item
import qualified Http.Handler                   as Handler
import           Http.Params
import           Http.Routes.Version
import           Servant
import           Services.Items

type ItemsAPI =
  ApiVersion :> "items" :> QueryParam "brand" BrandNameParam :> Get '[JSON] [Item]

itemsServer :: Items IO -> Server ItemsAPI
itemsServer = Handler.findItems
