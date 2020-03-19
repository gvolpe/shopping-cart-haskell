{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple
import           Domain.Brand
import qualified Services.Brands               as SB
import qualified Services.Items                as SI

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
  --createBrands brands (BrandName "Ibanez")
  bs     <- SB.findAll brands
  print bs
  is <- SI.findAll items
  print is

main :: IO ()
main = program
