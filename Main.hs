{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Algebras.Brands
import           Database.PostgreSQL.Simple
import           Domain.Brand

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
    brands <- mkLiveBrands conn
    --createBrands brands (BrandName "Ibanez")
    bs     <- findAllBrands brands
    print bs

main :: IO ()
main = program
