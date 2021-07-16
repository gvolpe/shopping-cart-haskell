{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Orphan where

import           Control.Monad.Catch            ( Exception )
import           Data.Bifunctor                 ( first )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple.FromField
import qualified Database.Redis                as R
import           Domain.Cart                    ( CartItem )
import           GHC.TypeNats                   ( KnownNat )
import           Refined
import           Servant

instance Exception R.Reply

instance KnownNat n => Predicate (SizeEqualTo n) Int where
  validate p value = validate p (digits value)   where
    digits :: Integral x => x -> [x]
    digits 0 = []
    digits x = digits (x `div` 10) ++ [x `mod` 10]

instance FromHttpApiData (Refined NonEmpty Text) where
  parseUrlPiece = first (T.pack . show) . refine

instance FromField [CartItem] where
  fromField = fromJSONField
