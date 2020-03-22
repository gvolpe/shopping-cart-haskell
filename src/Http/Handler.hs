{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Http.Handler where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Foldable                  ( traverse_ )
import qualified Data.Map                      as M
import           Data.Monoid                    ( (<>) )
import qualified Data.UUID                     as UUID
import           Domain.Brand
import           Domain.Cart
import           Domain.Item
import           Domain.User
import           Http.Params
import           Logger
import           Servant
import           Services.Brands                ( Brands )
import qualified Services.Brands               as SB
import           Services.Items                 ( Items )
import qualified Services.Items                as SI
import           Services.ShoppingCart          ( ShoppingCart )
import qualified Services.ShoppingCart         as SC

findBrands :: Brands IO -> Handler [Brand]
findBrands b = do
  logInfo "[Brands] - Find all"
  liftIO $ SB.findAll b

findItems :: Items IO -> Maybe BrandNameParam -> Handler [Item]
findItems i (Just bn) = do
  logInfo $ "[Items] - Find by brand: " <> unBrandName brand
  liftIO $ SI.findBy i brand
  where brand = toBrandName bn
findItems i Nothing = do
  logInfo "[Items] - Find all"
  liftIO $ SI.findAll i

findCartBy :: ShoppingCart IO -> UserId -> Handler CartTotal
findCartBy s uid@UserId {..} = do
  logInfo $ "[Shopping Cart] - Find by UserId: " <> UUID.toText unUserId
  liftIO $ SC.get s uid

addToCart :: ShoppingCart IO -> UserId -> Cart -> Handler ()
addToCart s uid@UserId {..} Cart {..} = do
  logInfo $ "[Shopping Cart] - Add items for UserId: " <> UUID.toText unUserId
  liftIO $ traverse_ (\(i, q) -> SC.add s uid i q) (M.toList unCart)

deleteCartBy :: ShoppingCart IO -> UserId -> Handler ()
deleteCartBy s uid@UserId {..} = do
  logInfo $ "[Shopping Cart] - Delete Cart by UserId: " <> UUID.toText unUserId
  liftIO $ SC.delete s uid
