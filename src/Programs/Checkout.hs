{-# LANGUAGE DeriveAnyClass, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Programs.Checkout
  ( Checkout(..)
  , mkCheckout
  )
where

import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Retry
import           Data.Functor                   ( void )
import qualified Data.Text                     as T
import qualified Data.UUID                     as UUID
import           Domain.Cart
import           Domain.Checkout
import           Domain.Item
import           Domain.Order
import           Domain.Payment
import           Domain.User
import           Effects.Logger
import           Http.Clients.Payments          ( PaymentClient )
import qualified Http.Clients.Payments         as PC
import           Services.Orders                ( Orders )
import qualified Services.Orders               as SO
import           Services.ShoppingCart          ( ShoppingCart )
import qualified Services.ShoppingCart         as SC

import           Control.Monad.Catch
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           UnliftIO.Async                 ( async )
import           UnliftIO.Concurrent            ( forkIO )

data Checkout m = Checkout
  { process :: UserId -> Card -> m OrderId
  }

mkCheckout
  :: (Logger m, MonadIO m, MonadMask m, MonadUnliftIO m)
  => PaymentClient m
  -> ShoppingCart m
  -> Orders m
  -> m (Checkout m)
mkCheckout p s o = pure $ Checkout (process' p s o)

policy :: Monad m => RetryPolicyM m
policy = limitRetries 3 <> exponentialBackoff 10000

processPayment'
  :: (Logger m, MonadIO m, MonadMask m)
  => PaymentClient m
  -> Payment
  -> m PaymentId
processPayment' client payment = recoverAll policy action
 where
  action RetryStatus {..} = do
    logInfo $ "[Checkout] - Processing payment #" <> T.pack (show rsIterNumber)
    PC.processPayment client payment

data OrderError = OrderError deriving (Exception, Show)
data PaymentError = PaymentError deriving (Exception, Show)

-- TODO: Run in the background once the number of retries has been reached
createOrder'
  :: forall m
   . (Logger m, MonadIO m, MonadMask m, MonadUnliftIO m)
  => Orders m
  -> UserId
  -> PaymentId
  -> [CartItem]
  -> Money
  -> m OrderId
createOrder' orders uid pid items total =
  foo
    `catch` (\(SomeException e) -> do
              logError "Failed after some attempts"
              void $ async foo
              throwM OrderError :: m OrderId
            )
 where
  foo = recoverAll policy action
  action RetryStatus {..} = do
    logInfo $ "[Checkout] - Creating order #" <> T.pack (show rsIterNumber)
    SO.create orders uid pid items total

logWith :: Logger m => T.Text -> UserId -> m ()
logWith t UserId {..} = logInfo $ t <> UUID.toText unUserId

process'
  :: (Logger m, MonadIO m, MonadMask m, MonadUnliftIO m)
  => PaymentClient m
  -> ShoppingCart m
  -> Orders m
  -> UserId
  -> Card
  -> m OrderId
process' pc sc so uid card = do
  logWith "[Checkout] - Retrieving shopping cart for " uid
  CartTotal {..} <- SC.get sc uid
  paymentId      <- processPayment' pc (payment cartTotal)
  orderId        <- createOrder' so uid paymentId cartItems cartTotal
  logWith "[Checkout] - Deleting shopping cart for " uid
  SC.delete sc uid
  pure orderId
  where payment t = Payment uid t card
