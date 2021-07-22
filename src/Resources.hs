{-# LANGUAGE OverloadedStrings #-}

module Resources
  ( Resources(..)
  , mkResources
  )
where

import           Control.Monad.Catch            ( bracket )
import           Control.Monad.Managed
import qualified Database.PostgreSQL.Simple    as P
import qualified Database.Redis                as R
import           Effects.Logger

data Resources = Res
  { psql :: P.Connection
  , redis :: R.Connection
  }

mkResources :: Managed Resources
mkResources = Res <$> psqlResource <*> redisResource

redisResource :: Managed R.Connection
redisResource =
  let acquire = do
        logInfo "Acquiring Redis connection"
        R.checkedConnect R.defaultConnectInfo
      release c = do
        logInfo "Closing Redis connection"
        R.disconnect c
  in managed $ bracket acquire release

psqlResource :: Managed P.Connection
psqlResource =
  let acquire = do
        logInfo "Acquiring PostgreSQL connection"
        P.connect P.ConnectInfo { P.connectHost     = "localhost"
                                , P.connectPort     = 5432
                                , P.connectUser     = "postgres"
                                , P.connectPassword = "my-password"
                                , P.connectDatabase = "store"
                                }
      release c = do
        logInfo "Closing PostgreSQL connection"
        P.close c
  in  managed $ bracket acquire release
