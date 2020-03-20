{-# LANGUAGE DeriveGeneric #-}

module Domain.User where

import           Data.UUID                      ( UUID )
import           GHC.Generics                   ( Generic )

newtype UserId = UserId {
 unUserId :: UUID
} deriving (Generic, Show)
