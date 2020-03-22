{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Http.Server (runServer) where

import           Data.Aeson
import           Data.Proxy
import           Http.Routes.Brands
import           Http.Routes.Cart
import           Http.Routes.Items
import           Servant
import           Servant.API
import           Network.Wai                    ( Application )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Cors    ( simpleCors )
import           Services

type API = BrandsAPI :<|> ItemsAPI :<|> CartAPI

server :: Services IO -> Server API
server s = brandsServer (brands s) :<|> itemsServer (items s) :<|> cartServer (cart s)

api :: Services IO -> Application
api s = serve (Proxy :: Proxy API) (server s)

runServer :: Services IO -> IO ()
runServer s = do
  putStrLn "Started server on localhost:8080"
  run 8080 $ simpleCors (api s)
