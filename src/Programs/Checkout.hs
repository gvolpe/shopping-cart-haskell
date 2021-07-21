{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels, TemplateHaskell #-}

module Programs.Checkout
  ( Checkout(..)
  , mkCheckout
  )
where

import           Control.Lens                   ( (^.)
                                                , (&)
                                                )
import           Control.Monad.Catch
import           Control.Retry                  ( RetryPolicyM
                                                , RetryStatus(..)
                                                , limitRetries
                                                , exponentialBackoff
                                                )
import           Data.Generics.Labels           ( )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.Text                     as T
import           Domain.Cart
import           Domain.Checkout
import           Domain.Item
import           Domain.Order
import           Domain.Payment
import           Domain.User
import           Effects.Background
import           Effects.Logger
import           Effects.Retry                  ( Retry(..) )
import           GHC.Generics                   ( Generic )
import           Http.Clients.Payments          ( PaymentClient )
import           Refined                 hiding ( NonEmpty )
import           Services.Orders                ( Orders )
import           Services.ShoppingCart          ( ShoppingCart )
import           Utils.Errors                   ( attempt
                                                , ensureNonEmpty
                                                )

data Checkout m = Checkout
  { process :: UserId -> Card -> m OrderId
  } deriving Generic

mkCheckout
  :: (Background m, Logger m, MonadMask m, Retry m)
  => PaymentClient m
  -> ShoppingCart m
  -> Orders m
  -> Checkout m
mkCheckout p s o = Checkout (process' p s o)

policy :: Monad m => RetryPolicyM m
policy = limitRetries 3 <> exponentialBackoff 10000

process'
  :: forall m
   . (Background m, Logger m, MonadMask m, Retry m)
  => PaymentClient m
  -> ShoppingCart m
  -> Orders m
  -> UserId
  -> Card
  -> m OrderId
process' payments cart orders uid card = do
  CartTotal {..} <- uid & cart ^. #get
  its            <- ensureNonEmpty EmptyCartError items
  pid            <- processPayment' $ Payment uid total card
  oid            <- createOrder' pid its total
  oid <$ attempt (uid & cart ^. #delete)
 where
  -- Process payment with retries
  processPayment' :: Payment -> m PaymentId
  processPayment' payment =
    let action RetryStatus {..} = do
          logInfo $ "[Checkout] - Processing payment #" <> T.pack
            (show rsIterNumber)
          payment & payments ^. #process
    in  retry policy action
  -- Create orders with retries and background action
  createOrder' :: PaymentId -> NonEmpty CartItem -> Money -> m OrderId
  createOrder' pid items' total' =
    let bgAction :: m OrderId -> m OrderId
        bgAction fa = fa `onError` do
          logError "[Checkout] - Failed to create order, rescheduling"
          schedule (bgAction fa) (Mins $$(refineTH 60))
          throwM OrderError
        action RetryStatus {..} = do
          logInfo $ "[Checkout] - Creating order #" <> T.pack
            (show rsIterNumber)
          (orders ^. #create) uid pid items' total'
    in  bgAction $ retry policy action
