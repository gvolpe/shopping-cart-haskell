{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple
import           Domain.Brand
import           Http.Server
import qualified Services.Brands               as SB
import qualified Services.Items                as SI
import           Services

sqlInfo :: ConnectInfo
sqlInfo = ConnectInfo { connectHost     = "localhost"
                      , connectPort     = 5432
                      , connectUser     = "postgres"
                      , connectPassword = ""
                      , connectDatabase = "store"
                      }

program :: IO ()
program = do
  putStrLn "Acquiring PSQL connection"
  conn   <- connect sqlInfo
  brands <- SB.mkLiveBrands conn
  items  <- SI.mkLiveItems conn
  runServer (Services brands items)
  --createBrands brands (BrandName "Ibanez")
  --bs     <- SB.findAll brands
  --print bs
  --is <- SI.findAll items
  --print is

main :: IO ()
main = program
