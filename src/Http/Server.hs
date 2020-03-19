{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Http.Server (runServer) where

import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           Domain.Brand
import qualified Http.Handler                   as Handler
import           Servant
import           Servant.API
import           Network.Wai                    ( Application )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Cors    ( simpleCors )
import           Services

type ApiVersion = "v1"

type API =
       ApiVersion :> "brands" :> Get '[JSON] [Brand]
  :<|> ApiVersion :> "items" :> Get '[JSON] String

server :: Services IO -> Server API
server s = Handler.findAllBrands (brands s) :<|> Handler.findAllItems (items s)

api :: Services IO -> Application
api s = serve (Proxy :: Proxy API) (server s)

runServer :: Services IO -> IO ()
runServer s = do
  putStrLn "Started server on localhost:8080"
  run 8080 $ simpleCors (api s)
