{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Http.Server (runServer) where

import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           Domain.Brand
import           Domain.Item
import qualified Http.Handler                   as Handler
import           Http.Params
import           Servant
import           Servant.API
import           Network.Wai                    ( Application )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Cors    ( simpleCors )
import           Services

type ApiVersion = "v1"

type BrandsAPI =
       ApiVersion :> "brands" :> Get '[JSON] [Brand]

type ItemsAPI =
       ApiVersion :> "items" :> QueryParam "brand" BrandNameParam :> Get '[JSON] [Item]

type API = BrandsAPI :<|> ItemsAPI

server :: Services IO -> Server API
server s = Handler.findBrands (brands s) :<|> Handler.findItems (items s)

api :: Services IO -> Application
api s = serve (Proxy :: Proxy API) (server s)

runServer :: Services IO -> IO ()
runServer s = do
  putStrLn "Started server on localhost:8080"
  run 8080 $ simpleCors (api s)
