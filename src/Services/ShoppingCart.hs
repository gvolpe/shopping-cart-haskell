{-# LANGUAGE DeriveGeneric, OverloadedLabels, OverloadedStrings #-}

module Services.ShoppingCart
  ( ShoppingCart(..)
  , mkShoppingCart
  )
where

import           Control.Lens
import qualified Data.ByteString.Char8         as C
import           Data.Functor                   ( void )
import           Data.Generics.Labels           ( )
import qualified Data.UUID                     as UUID
import           Data.Witherable
import           Database.Redis                 ( Connection )
import qualified Database.Redis                as R
import           Domain.Cart             hiding ( items )
import           Domain.Item
import           Domain.User
import           GHC.Generics                   ( Generic )
import           Services.Items                 ( Items )
import           Utils.Errors                   ( liftMaybe )
import qualified Utils.Redis                   as R
import           Utils.Text                     ( logWith )

data ShoppingCart m = ShoppingCart
  { add :: UserId -> ItemId -> Quantity -> m ()
  , get :: UserId -> m CartTotal
  , delete :: UserId -> m ()
  , removeItem :: UserId -> ItemId -> m ()
  , update :: UserId -> Cart -> m ()
  } deriving Generic

mkShoppingCart :: Connection -> Items IO -> CartExpiration -> ShoppingCart IO
mkShoppingCart c i exp' = ShoppingCart { add        = add' c exp'
                                       , get        = get' c i
                                       , delete     = delete' c
                                       , removeItem = removeItem' c
                                       , update     = update' c exp'
                                       }

add' :: Connection -> CartExpiration -> UserId -> ItemId -> Quantity -> IO ()
add' conn (CartExpiration exp') (UserId uid) (ItemId i) (Quantity q) =
  R.runRedis conn $ do
    R.hset k f v & void
    R.expire k exp' & void
 where
  k = C.pack $ UUID.toString uid
  f = C.pack $ UUID.toString i
  v = C.pack $ show q

get' :: Connection -> Items IO -> UserId -> IO CartTotal
get' conn items u@(UserId uid) = do
  logWith "[Checkout] - Retrieving shopping cart for " u
  res <- R.runRedisM conn $ R.hgetall (R.writeUUID uid)
  its <- wither
    (\(k, v) -> do
      it <- R.readUUID ItemId k & liftMaybe
      qt <- R.readInt Quantity v & liftMaybe
      (<&> (`CartItem` qt)) <$> (it & items ^. #findById)
    )
    res
  pure (CartTotal its $ foldMap subTotal its)

delete' :: Connection -> UserId -> IO ()
delete' conn u@(UserId uid) = do
  logWith "[Checkout] - Deleting shopping cart for " u
  R.runRedis conn . void $ R.del [C.pack $ UUID.toString uid]

removeItem' :: Connection -> UserId -> ItemId -> IO ()
removeItem' conn (UserId uid) (ItemId i) =
  let k = C.pack $ UUID.toString uid
      f = C.pack $ UUID.toString i
  in  R.runRedis conn . void $ R.hdel k [f]

-- TODO: implement
update' :: Connection -> CartExpiration -> UserId -> Cart -> IO ()
update' = undefined
