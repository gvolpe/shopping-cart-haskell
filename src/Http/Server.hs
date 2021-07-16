{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Http.Server
  ( runServer
  )
where

import           Data.Proxy
import           Effects.Logger
import           Http.Routes.Brands
import           Http.Routes.Cart
import           Http.Routes.Checkout
import           Http.Routes.Items
import           Http.Routes.Orders
import           Servant
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Cors    ( simpleCors )
import           Services

type API = BrandsAPI :<|> ItemsAPI :<|> CartAPI :<|> CheckoutAPI :<|> OrdersAPI

server :: Services IO -> Server API
server Services {..} =
  brandsServer brands :<|> itemsServer items :<|>
  cartServer cart :<|> checkoutServer checkout :<|>
  ordersServer orders

api :: Services IO -> Application
api s = serve (Proxy :: Proxy API) (server s)

runServer :: Services IO -> IO ()
runServer s = do
  logInfo "Started server on localhost:8080"
  run 8080 $ simpleCors (api s)
