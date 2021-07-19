{-# LANGUAGE OverloadedStrings #-}

module Resources
  ( Resources(..)
  , mkResources
  )
where

import           Control.Monad.IO.Class
import qualified Database.PostgreSQL.Simple    as P
import qualified Database.Redis                as R
import           Effects.Logger

data Resources = Res
  { psql :: P.Connection
  , redis :: R.Connection
  }

mkResources :: (Logger m, MonadIO m) => m Resources
mkResources = Res <$> psqlResource <*> redisResource

redisResource :: (Logger m, MonadIO m) => m R.Connection
redisResource = do
  logInfo "Acquiring Redis connection"
  liftIO $ R.checkedConnect R.defaultConnectInfo

psqlResource :: (Logger m, MonadIO m) => m P.Connection
psqlResource = do
  logInfo "Acquiring PostgreSQL connection"
  liftIO $ P.connect P.ConnectInfo { P.connectHost     = "localhost"
                                   , P.connectPort     = 5432
                                   , P.connectUser     = "postgres"
                                   , P.connectPassword = "my-password"
                                   , P.connectDatabase = "store"
                                   }
