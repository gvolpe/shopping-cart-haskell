{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Http.Routes.Checkout where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Foldable                  ( traverse_ )
import qualified Data.Map                      as M
import qualified Data.UUID                     as UUID
import           Domain.Checkout
import           Domain.Order
import           Domain.User
import           Http.Params
import           Http.Routes.Version
import           Logger
import           Programs.Checkout              ( Checkout )
import qualified Programs.Checkout             as PC
import           Servant

-- TODO: it should be authenticated via JWT
type CheckoutAPI =
  ApiVersion :> "checkout" :> Capture "id" UserId :> ReqBody '[JSON] Card :> Post '[JSON] OrderId

checkoutServer :: Checkout IO -> Server CheckoutAPI
checkoutServer = checkout'

checkout' :: Checkout IO -> UserId -> Card -> Handler OrderId
checkout' s uid@UserId {..} card = do
  logInfo $ "[Checkout] - Processing order for UserId: " <> UUID.toText unUserId
  liftIO $ PC.process s uid card
