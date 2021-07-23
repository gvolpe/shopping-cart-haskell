{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Services.Orders
  ( Orders(..)
  , mkOrders
  )
where

import           Data.Aeson                     ( toJSON )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( listToMaybe )
import           Data.UUID                      ( UUID )
import qualified Data.UUID.V4                  as UUID
import           Database.PostgreSQL.Resilient  ( ResilientConnection(..) )
import           Database.PostgreSQL.Simple
import           Domain.Cart
import           Domain.Item
import           Domain.Order
import           Domain.Payment
import           Domain.User
import           GHC.Generics                   ( Generic )
import           Orphan                         ( )

data Orders m = Orders
  { get :: UserId -> OrderId -> m (Maybe Order)
  , findBy :: UserId -> m [Order]
  , create :: UserId -> PaymentId -> NonEmpty CartItem -> Money -> m OrderId
  } deriving Generic

mkOrders :: ResilientConnection IO -> Orders IO
mkOrders p = Orders
  { get    = \uid oid -> (fmap . fmap) toDomain (get' p uid oid)
  , findBy = (fmap . fmap) toDomain . findBy' p
  , create = create' p
  }

data OrderDTO = OrderDTO
  { _orderId :: UUID
  , _orderUserId :: UUID
  , _orderPaymentId :: UUID
  , _orderItems :: [CartItem]
  , _orderTotal :: Rational
  } deriving (Generic, FromRow, Show)

toDomain :: OrderDTO -> Order
toDomain OrderDTO {..} = Order { orderId        = OrderId _orderId
                               , orderPaymentId = PaymentId _orderPaymentId
                               , orderItems     = parseItems _orderItems
                               , orderTotal = Money $ fromRational _orderTotal
                               }

parseItems :: [CartItem] -> Map ItemId Quantity
parseItems xs = M.fromList $ (\i -> (itemId $ item i, quantity i)) <$> xs

selectByUserAndOrderQuery :: Query
selectByUserAndOrderQuery =
  "SELECT * FROM orders WHERE user_id = ? AND uuid = ?"

get' :: ResilientConnection IO -> UserId -> OrderId -> IO (Maybe OrderDTO)
get' pool (UserId uid) (OrderId oid) = do
  conn <- getConnection pool
  listToMaybe <$> query conn selectByUserAndOrderQuery [uid, oid]

findBy' :: ResilientConnection IO -> UserId -> IO [OrderDTO]
findBy' pool uid = do
  conn <- getConnection pool
  query conn "SELECT * FROM orders WHERE user_id = ?" uid

create'
  :: ResilientConnection IO
  -> UserId
  -> PaymentId
  -> NonEmpty CartItem
  -> Money
  -> IO OrderId
create' pool (UserId uid) (PaymentId pid) its (Money money) = do
  oid <- OrderId <$> UUID.nextRandom
  conn <- getConnection pool
  oid <$ executeMany conn
                     "INSERT INTO orders VALUES (?, ?, ?, ?, ?)"
                     (values oid)
  where values (OrderId oid) = [(oid, uid, pid, toJSON its, money)]
