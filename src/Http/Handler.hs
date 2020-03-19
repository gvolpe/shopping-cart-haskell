module Http.Handler where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Catch            ( handle )
import           Domain.Brand
import           Domain.Item
import           Http.Params
import           Servant
import           Servant.API
import           Services.Brands                ( Brands )
import qualified Services.Brands               as SB
import           Services.Items                 ( Items )
import qualified Services.Items                as SI

findBrands :: Brands IO -> Handler [Brand]
findBrands = liftIO . SB.findAll

findItems :: Items IO -> Maybe BrandNameParam -> Handler [Item]
findItems i (Just bn) = liftIO $ SI.findBy i (toBrandName bn)
findItems i Nothing   = liftIO $ SI.findAll i
