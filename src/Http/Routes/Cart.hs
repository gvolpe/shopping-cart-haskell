{-# LANGUAGE DataKinds, OverloadedLabels, OverloadedStrings, TypeOperators #-}

module Http.Routes.Cart where

import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Foldable                  ( traverse_ )
import           Data.Generics.Labels           ( )
import qualified Data.Map                      as M
import           Domain.Cart
import           Domain.User
import           Http.Routes.Version
import           Servant
import           Services.ShoppingCart          ( ShoppingCart )
import           Utils.Text                     ( logWith )

-- TODO: it should be authenticated via JWT
type CartAPI =
  ApiVersion :> "cart" :> Capture "id" UserId :> Get '[JSON] CartTotal :<|>
  ApiVersion :> "cart" :> Capture "id" UserId :> ReqBody '[JSON] Cart :> Post '[JSON] () :<|>
  ApiVersion :> "cart" :> Capture "id" UserId :> Delete '[JSON] ()

cartServer :: ShoppingCart IO -> Server CartAPI
cartServer s = findCartBy s :<|> addToCart s :<|> deleteCartBy s

findCartBy :: ShoppingCart IO -> UserId -> Handler CartTotal
findCartBy cart uid = do
  logWith "[Shopping Cart] - Find by UserId: " uid
  uid & cart ^. #get & liftIO

addToCart :: ShoppingCart IO -> UserId -> Cart -> Handler ()
addToCart cart uid (Cart ct) = do
  logWith "[Shopping Cart] - Add items for UserId: " uid
  traverse_ (uncurry $ (cart ^. #add) uid) (M.toList ct) & liftIO

deleteCartBy :: ShoppingCart IO -> UserId -> Handler ()
deleteCartBy cart uid = do
  logWith "[Shopping Cart] - Delete Cart by UserId: " uid
  uid & cart ^. #delete & liftIO
