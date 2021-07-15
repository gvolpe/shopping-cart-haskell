{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Refined.Instances where

import           Data.Typeable                  ( typeOf )
import           Refined
import           GHC.TypeNats                   ( KnownNat )

instance KnownNat n => Predicate (SizeEqualTo n) Int where
  validate p value = validate p (digits value)   where
    digits :: Integral x => x -> [x]
    digits 0 = []
    digits x = digits (x `div` 10) ++ [x `mod` 10]
