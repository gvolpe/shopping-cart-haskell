{-# LANGUAGE RecordWildCards #-}

module Services.ShoppingCart where

import qualified Data.ByteString.Lazy.UTF8     as B
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Char8         as C
import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import qualified Data.UUID                     as UUID
import           Data.Witherable
import           Database.Redis                 ( Connection )
import qualified Database.Redis                as R
import           Domain.Cart
import           Domain.Item
import           Domain.User
import           Services.Items                 ( Items )
import qualified Services.Items                as SI
import           Text.Read                      ( readMaybe )
import           UnliftIO.Exception             ( Exception
                                                , fromEitherM
                                                )
import Utils.Lift (liftMaybe)
import           Utils.Text                     ( normalizeBS )

data ShoppingCart m = ShoppingCart
  { add :: UserId -> ItemId -> Quantity -> m ()
  , get :: UserId -> m CartTotal
  , delete :: UserId -> m ()
  , removeItem :: UserId -> ItemId -> m ()
  , update :: UserId -> Cart -> m ()
  }

mkShoppingCart
  :: Connection -> Items IO -> CartExpiration -> IO (ShoppingCart IO)
mkShoppingCart c i exp = pure $ ShoppingCart { add        = add' c exp
                                             , get        = get' c i
                                             , delete     = delete' c
                                             , removeItem = removeItem' c
                                             , update     = update' c exp
                                             }

add' :: Connection -> CartExpiration -> UserId -> ItemId -> Quantity -> IO ()
add' conn CartExpiration {..} u i q = R.runRedis conn $ do
  R.hset k f v
  void $ R.expire k unCartExpiration
 where
  k = C.pack . UUID.toString $ unUserId u
  f = C.pack . UUID.toString $ unItemId i
  v = C.pack $ show q

instance Exception R.Reply

calcTotal :: [CartItem] -> Money
calcTotal = foldMap
  (\CartItem {..} ->
    (itemPrice cartItem) * (Money . fromIntegral $ unQuantity cartQuantity)
  )

get' :: Connection -> Items IO -> UserId -> IO CartTotal
get' conn items u = do
  res <- fromEitherM
    (R.runRedis conn $ R.hgetall (C.pack . UUID.toString $ unUserId u))
  its <- wither
    (\(k, v) -> do
      it <- liftMaybe (ItemId <$> UUID.fromText (normalizeBS k))
      qt <- liftMaybe (Quantity <$> (readMaybe . B.toString $ B.fromStrict v))
      (<&> (`CartItem` qt)) <$> SI.findById items it
    )
    res
  pure (CartTotal its $ calcTotal its)

delete' :: Connection -> UserId -> IO ()
delete' conn u =
  R.runRedis conn . void $ R.del [C.pack . UUID.toString $ unUserId u]

removeItem' :: Connection -> UserId -> ItemId -> IO ()
removeItem' conn u i = R.runRedis conn . void $ R.hdel k [f]
 where
  k = C.pack . UUID.toString $ unUserId u
  f = C.pack . UUID.toString $ unItemId i

-- TODO: implement
update' :: Connection -> CartExpiration -> UserId -> Cart -> IO ()
update' = undefined
