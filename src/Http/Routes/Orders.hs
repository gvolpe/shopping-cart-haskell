{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Http.Routes.Orders where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Foldable                  ( traverse_ )
import qualified Data.Map                      as M
import qualified Data.UUID                     as UUID
import           Domain.Order
import           Domain.User
import           Effects.Logger
import           Http.Params
import           Http.Routes.Version
import           Servant
import           Services.Orders                ( Orders )
import qualified Services.Orders               as SO

-- TODO: it should be authenticated via JWT
type OrdersAPI =
  ApiVersion :> "orders" :> Capture "id" UserId :> Get '[JSON] [Order] -- :<|>
  --ApiVersion :> "cart" :> Capture "id" UserId :> Delete '[JSON] ()

ordersServer :: Orders IO -> Server OrdersAPI
ordersServer = findOrdersBy

findOrdersBy :: Orders IO -> UserId -> Handler [Order]
findOrdersBy s uid@UserId {..} = do
  logInfo $ "[Orders] - Find by UserId: " <> UUID.toText unUserId
  liftIO $ SO.findBy s uid
