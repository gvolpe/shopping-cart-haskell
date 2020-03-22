{-# LANGUAGE DataKinds, TypeOperators #-}

module Http.Routes.Cart where

import           Data.UUID
import           Domain.Cart
import           Domain.User
import qualified Http.Handler                   as Handler
import           Http.Params
import           Http.Routes.Version
import           Servant
import           Services.ShoppingCart

-- TODO: it should be authenticated via JWT
type CartAPI =
  ApiVersion :> "cart" :> Capture "id" UserId :> Get '[JSON] CartTotal

cartServer :: ShoppingCart IO -> Server CartAPI
cartServer = Handler.findCartBy
