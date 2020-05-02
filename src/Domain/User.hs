{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Domain.User where

import           Data.Aeson
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData )

newtype UserId = UserId UUID deriving (Eq, Generic, Ord, FromHttpApiData, Show)

instance ToRow UserId

instance ToJSON UserId where
  toJSON (UserId i) = toJSON i
