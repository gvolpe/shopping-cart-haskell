{-# LANGUAGE OverloadedStrings #-}

module Resources
  ( Resources(..)
  , mkResources
  )
where

import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple
import           Logger

-- Soon there will be more resources, e.g. Redis connection
data Resources = Resources
  { psql :: Connection
  }

mkResources :: (Logger m, MonadIO m) => m Resources
mkResources = Resources <$> psqlResource

psqlResource :: (Logger m, MonadIO m) => m Connection
psqlResource = do
  logInfo "Acquiring PostgreSQL connection"
  liftIO $ connect ConnectInfo { connectHost     = "localhost"
                               , connectPort     = 5432
                               , connectUser     = "postgres"
                               , connectPassword = ""
                               , connectDatabase = "store"
                               }
