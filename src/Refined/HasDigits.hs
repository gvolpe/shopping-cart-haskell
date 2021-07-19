{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Refined.HasDigits where

import           Data.Typeable                  ( typeOf )
import           GHC.Generics                   ( Generic )
import           GHC.TypeLits                   ( KnownNat
                                                , Nat
                                                )
import           Orphan                         ( )
import           Refined
import           Refined.Helper                 ( i2text
                                                , nv
                                                )

data HasDigits (n :: Nat) = HasDigits deriving Generic

instance (Integral x, Show x, KnownNat n) => Predicate (HasDigits n) x where
  validate p x = do
    let n = fromIntegral (nv @n)
    if n == toInteger (length $ show x)
      then Nothing
      else throwRefineOtherException
        (typeOf p)
        ("Invalid number of digits. Expected " <> i2text n)
