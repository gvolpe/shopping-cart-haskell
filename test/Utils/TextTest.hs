{-# LANGUAGE LambdaCase, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Utils.TextTest where

import           Data.Char                      ( isLower
                                                , isUpper
                                                )
import qualified Data.Text                     as T
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Utils.Text                     ( capitalize )

prop_capitalize :: Property
prop_capitalize = property $ do
  (forAll $ (capitalize . T.pack) <$> Gen.list (Range.linear 0 100) Gen.alpha)
    >>= \case
          (T.unpack -> x : xs) -> isUpper x === all isLower xs
          (T.unpack -> _     ) -> pure ()

utilTextTests :: Group
utilTextTests = $$(discover)
