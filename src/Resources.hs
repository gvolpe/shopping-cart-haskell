{-# LANGUAGE OverloadedStrings #-}

module Resources
  ( Resources(..)
  , mkResources
  )
where

import           Control.Monad.Catch
import           Control.Monad.Managed
import qualified Data.Text                     as T
import           Database.PostgreSQL.Resilient
import qualified Database.PostgreSQL.Simple    as P
import qualified Database.Redis                as R
import           Effects.Logger

data Resources = Res
  { psql :: ResilientConnection IO
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
  in  managed $ bracket acquire release

psqlResource :: Managed (ResilientConnection IO)
psqlResource = managed $ withResilientConnection
  defaultSettings
  logHandler
  P.ConnectInfo { P.connectHost     = "localhost"
                , P.connectPort     = 5432
                , P.connectUser     = "postgres"
                , P.connectPassword = "my-password"
                , P.connectDatabase = "store"
                }
 where
  logHandler :: String -> IO ()
  logHandler = logInfo . T.pack

