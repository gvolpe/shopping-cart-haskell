{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Http.Routes.Cart where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Foldable                  ( traverse_ )
import qualified Data.Map                      as M
import qualified Data.UUID                     as UUID
import           Domain.Cart
import           Domain.User
import           Effects.Logger
import           Http.Routes.Version
import           Servant
import           Services.ShoppingCart          ( ShoppingCart )
import qualified Services.ShoppingCart         as SC

-- TODO: it should be authenticated via JWT
type CartAPI =
  ApiVersion :> "cart" :> Capture "id" UserId :> Get '[JSON] CartTotal :<|>
  ApiVersion :> "cart" :> Capture "id" UserId :> ReqBody '[JSON] Cart :> Post '[JSON] () :<|>
  ApiVersion :> "cart" :> Capture "id" UserId :> Delete '[JSON] ()

cartServer :: ShoppingCart IO -> Server CartAPI
cartServer s = findCartBy s :<|> addToCart s :<|> deleteCartBy s

findCartBy :: ShoppingCart IO -> UserId -> Handler CartTotal
findCartBy s u@(UserId uid) = do
  logInfo $ "[Shopping Cart] - Find by UserId: " <> UUID.toText uid
  liftIO $ SC.get s u

addToCart :: ShoppingCart IO -> UserId -> Cart -> Handler ()
addToCart s u@(UserId uid) (Cart cart) = do
  logInfo $ "[Shopping Cart] - Add items for UserId: " <> UUID.toText uid
  liftIO $ traverse_ (uncurry $ SC.add s u) (M.toList cart)

deleteCartBy :: ShoppingCart IO -> UserId -> Handler ()
deleteCartBy s u@(UserId uid) = do
  logInfo $ "[Shopping Cart] - Delete Cart by UserId: " <> UUID.toText uid
  liftIO $ SC.delete s u
