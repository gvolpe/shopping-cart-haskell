{-# LANGUAGE OverloadedStrings #-}

module Utils.Text where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

capitalize :: Text -> Text
capitalize "" = ""
capitalize cs =
  let h = toUpper (T.head cs)
      t = T.map toLower (T.tail cs)
  in  T.cons h t

normalize :: Text -> Text
normalize = capitalize . T.filter (/= '"')
