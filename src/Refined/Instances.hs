{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Refined.Instances where

import           Control.Monad                  ( when )
import           Data.Text                      ( Text
                                                , null
                                                , unpack
                                                )
import           Data.Typeable                  ( typeOf )
import           Language.Haskell.TH.Syntax     ( Lift(..) )
import           Prelude                 hiding ( null )
import           Refined

instance Predicate NonEmpty Text where
  --validate p value = validate p (unpack value) -- validate using String instance
  validate p value = when (null value)
    $ throwRefineOtherException (typeOf p) "Text cannot be empty"

-- Or use: https://hackage.haskell.org/package/th-lift-instances
instance Lift Text where
  lift t = lift (unpack t)
