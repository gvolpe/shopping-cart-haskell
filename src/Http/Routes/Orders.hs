{-# LANGUAGE DataKinds, OverloadedLabels, OverloadedStrings, TypeOperators #-}

module Http.Routes.Orders where

import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Generics.Labels           ( )
import qualified Data.UUID                     as UUID
import           Domain.Order
import           Domain.User
import           Effects.Logger
import           Http.Routes.Version
import           Servant
import           Services.Orders                ( Orders )

-- TODO: it should be authenticated via JWT
type OrdersAPI =
  ApiVersion :> "orders" :> Capture "id" UserId :> Get '[JSON] [Order] :<|>
  ApiVersion :> "orders" :> Capture "id" UserId :> Capture "oid" OrderId :> Get '[JSON] (Maybe Order)

ordersServer :: Orders IO -> Server OrdersAPI
ordersServer s = findAllOrdersBy s :<|> findOrderBy s

findAllOrdersBy :: Orders IO -> UserId -> Handler [Order]
findAllOrdersBy orders u@(UserId uid) = do
  logInfo $ "[Orders] - Find all by UserId: " <> UUID.toText uid
  u & orders ^. #findBy & liftIO

findOrderBy :: Orders IO -> UserId -> OrderId -> Handler (Maybe Order)
findOrderBy orders u@(UserId uid) oid = do
  logInfo $ "[Orders] - Find order for UserId: " <> UUID.toText uid
  (orders ^. #get) u oid & liftIO
