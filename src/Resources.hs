module Resources
  ( Resources(..)
  , mkResources
  )
where

import           Database.PostgreSQL.Simple

-- Soon there will be more resources, e.g. Redis connection
data Resources = Resources
  { psql :: Connection
  }

mkResources :: IO Resources
mkResources = Resources <$> connect psqlInfo

psqlInfo :: ConnectInfo
psqlInfo = ConnectInfo { connectHost     = "localhost"
                       , connectPort     = 5432
                       , connectUser     = "postgres"
                       , connectPassword = ""
                       , connectDatabase = "store"
                       }
