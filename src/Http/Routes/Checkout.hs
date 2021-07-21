{-# LANGUAGE DataKinds, OverloadedLabels, OverloadedStrings, TypeOperators #-}

module Http.Routes.Checkout where

import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Generics.Labels           ( )
import qualified Data.UUID                     as UUID
import           Domain.Checkout
import           Domain.Order
import           Domain.User
import           Effects.Logger
import           Http.Routes.Version
import           Programs.Checkout              ( Checkout )
import           Servant

-- TODO: it should be authenticated via JWT
type CheckoutAPI =
  ApiVersion :> "checkout" :> Capture "id" UserId :> ReqBody '[JSON] Card :> Post '[JSON] OrderId

checkoutServer :: Checkout IO -> Server CheckoutAPI
checkoutServer = checkout'

checkout' :: Checkout IO -> UserId -> Card -> Handler OrderId
checkout' checkout u@(UserId uid) card = do
  logInfo $ "[Checkout] - Processing order for UserId: " <> UUID.toText uid
  (checkout ^. #process) u card & liftIO
