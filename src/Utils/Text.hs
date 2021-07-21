{-# LANGUAGE OverloadedStrings #-}

module Utils.Text where

import qualified Data.ByteString.Char8         as C
import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding
import qualified Data.UUID                     as UUID
import           Domain.User
import           Effects.Logger

capitalize :: Text -> Text
capitalize "" = ""
capitalize cs =
  let h = toUpper (T.head cs)
      t = T.map toLower (T.tail cs)
  in  T.cons h t

normalize :: Text -> Text
normalize = capitalize . T.filter (/= '"')

normalizeBS :: C.ByteString -> Text
normalizeBS bs = T.filter (/= '"') (decodeUtf8 bs)

logWith :: Logger m => T.Text -> UserId -> m ()
logWith t (UserId uid) = logInfo $ t <> UUID.toText uid
