{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Utils.TextTest where

import           Data.Char                      ( isLower
                                                , isUpper
                                                )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

prop_capitalize :: Property
prop_capitalize = property $ do
  (forAll $ Gen.list (Range.linear 0 100) Gen.alpha) >>= \case
    (x : xs) -> (isUpper x && all isLower xs) === True

utilTextTests :: Group
utilTextTests = $$(discover)
