{-# LANGUAGE DataKinds, TypeOperators #-}

module Http.Routes.Cart where

import           Data.UUID
import           Domain.Cart
import           Domain.User
import qualified Http.Handler                   as H
import           Http.Params
import           Http.Routes.Version
import           Servant
import           Services.ShoppingCart

-- TODO: it should be authenticated via JWT
type CartAPI =
  ApiVersion :> "cart" :> Capture "id" UserId :> Get '[JSON] CartTotal :<|>
  ApiVersion :> "cart" :> Capture "id" UserId :> ReqBody '[JSON] Cart :> Post '[JSON] () :<|>
  ApiVersion :> "cart" :> Capture "id" UserId :> Delete '[JSON] ()

cartServer :: ShoppingCart IO -> Server CartAPI
cartServer s = H.findCartBy s :<|> H.addToCart s :<|> H.deleteCartBy s
