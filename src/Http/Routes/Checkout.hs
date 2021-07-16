{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Http.Routes.Checkout where

import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.UUID                     as UUID
import           Domain.Checkout
import           Domain.Order
import           Domain.User
import           Effects.Logger
import           Http.Routes.Version
import           Programs.Checkout              ( Checkout )
import qualified Programs.Checkout             as PC
import           Servant

-- TODO: it should be authenticated via JWT
type CheckoutAPI =
  ApiVersion :> "checkout" :> Capture "id" UserId :> ReqBody '[JSON] Card :> Post '[JSON] OrderId

checkoutServer :: Checkout IO -> Server CheckoutAPI
checkoutServer = checkout'

checkout' :: Checkout IO -> UserId -> Card -> Handler OrderId
checkout' s u@(UserId uid) card = do
  logInfo $ "[Checkout] - Processing order for UserId: " <> UUID.toText uid
  liftIO $ PC.process s u card
