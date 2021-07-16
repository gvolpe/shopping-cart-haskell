{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Services.Orders
  ( Orders(..)
  , mkOrders
  )
where

import           Data.Aeson                     ( toJSON )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( listToMaybe )
import           Data.UUID                      ( UUID )
import qualified Data.UUID.V4                  as UUID
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
  , create :: UserId -> PaymentId -> [CartItem] -> Money -> m OrderId
  }

mkOrders :: Connection -> IO (Orders IO)
mkOrders conn = pure $ Orders
  { get    = \uid oid -> (fmap . fmap) toDomain (get' conn uid oid)
  , findBy = (fmap . fmap) toDomain . findBy' conn
  , create = create' conn
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
parseItems xs =
  M.fromList $ (\i -> (itemId $ cartItem i, cartQuantity i)) <$> xs

selectByUserAndOrderQuery :: Query
selectByUserAndOrderQuery =
  "SELECT * FROM orders WHERE user_id = ? AND uuid = ?"

get' :: Connection -> UserId -> OrderId -> IO (Maybe OrderDTO)
get' conn (UserId uid) (OrderId oid) =
  listToMaybe <$> query conn selectByUserAndOrderQuery [uid, oid]

findBy' :: Connection -> UserId -> IO [OrderDTO]
findBy' = flip query "SELECT * FROM orders WHERE user_id = ?"

create'
  :: Connection -> UserId -> PaymentId -> [CartItem] -> Money -> IO OrderId
create' conn (UserId uid) (PaymentId pid) its (Money money) = do
  oid <- OrderId <$> UUID.nextRandom
  oid <$ executeMany conn "INSERT INTO orders VALUES (?, ?, ?, ?, ?)" (values oid)
  where values (OrderId oid) = [(oid, uid, pid, toJSON its, money)]
