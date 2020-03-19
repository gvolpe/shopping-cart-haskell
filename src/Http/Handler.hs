{-# LANGUAGE OverloadedStrings #-}

module Http.Handler where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Catch            ( handle )
import           Domain.Brand
import           Servant
import           Servant.API
import           Services.Brands                ( Brands )
import qualified Services.Brands               as SB
import           Services.Items                 ( Items )
import qualified Services.Items                as SI

findAllBrands :: Brands IO -> Handler [Brand]
findAllBrands = liftIO . SB.findAll

findAllItems :: Items IO -> Handler String
findAllItems _ = pure "TODO"
