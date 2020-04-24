{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Refined.Instances where

import           Control.Monad                  ( when )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Typeable                  ( typeOf )
import           Refined
import           GHC.TypeNats                   ( KnownNat )

instance Predicate NonEmpty Text where
  validate p value = when (T.null value)
    $ throwRefineOtherException (typeOf p) "Text cannot be empty"

instance KnownNat n => Predicate (SizeEqualTo n) Int where
  validate p value = validate p (digits value)   where
    digits :: Integral x => x -> [x]
    digits 0 = []
    digits x = digits (x `div` 10) ++ [x `mod` 10]
