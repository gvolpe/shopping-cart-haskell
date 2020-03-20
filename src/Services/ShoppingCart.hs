module Services.ShoppingCart where

import qualified Data.ByteString.Char8         as C
import           Data.Functor                   ( void )
import           Data.UUID                      ( toString )
import           Database.Redis                 ( Connection )
import qualified Database.Redis                as R
import           Domain.Cart
import           Domain.Item
import           Domain.User
import           Services.Items                 ( Items )

data ShoppingCart m = ShoppingCart
  { add :: UserId -> ItemId -> Quantity -> m ()
  , get :: UserId -> m CartTotal
  , delete :: UserId -> m ()
  , removeItem :: UserId -> ItemId -> m ()
  , update :: UserId -> Cart -> m ()
  }

mkShoppingCart :: Connection -> Items IO -> Expiration -> IO (ShoppingCart IO)
mkShoppingCart c i exp = pure $ ShoppingCart { add        = add' c exp
                                             , get        = get' c i
                                             , delete     = delete' c
                                             , removeItem = removeItem' c
                                             , update     = update' c exp
                                             }

add' :: Connection -> Expiration -> UserId -> ItemId -> Quantity -> IO ()
add' conn exp u i q = R.runRedis conn $ do
  R.hset k f v
  void $ R.expire k exp
 where
  k = C.pack . toString $ unUserId u
  f = C.pack . toString $ unItemId i
  v = C.pack $ show q

get' :: Connection -> Items IO -> UserId -> IO CartTotal
get' conn items u = do
  it <- R.runRedis conn $ R.hgetall (C.pack . toString $ unUserId u)
  -- TODO: Complete
  undefined

delete' :: Connection -> UserId -> IO ()
delete' conn u =
  R.runRedis conn . void $ R.del [C.pack . toString $ unUserId u]

removeItem' :: Connection -> UserId -> ItemId -> IO ()
removeItem' conn u i = R.runRedis conn . void $ R.hdel k [f]
 where
  k = C.pack . toString $ unUserId u
  f = C.pack . toString $ unItemId i

-- TODO: implement
update' :: Connection -> Expiration -> UserId -> Cart -> IO ()
update' = undefined
