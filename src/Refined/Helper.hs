{-# LANGUAGE MagicHash, ScopedTypeVariables, AllowAmbiguousTypes #-}

module Refined.Helper where

import           Control.Arrow                  ( left )
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as TextLazy
import qualified Data.Text.Lazy.Builder        as TextBuilder
import qualified Data.Text.Lazy.Builder.Int    as TextBuilder
import           GHC.Exts                       ( Proxy#
                                                , proxy#
                                                )
import           GHC.TypeLits                   ( KnownNat
                                                , natVal'
                                                )
import           Refined

infixl 9 .>

-- | Helper function, stolen from the flow package.
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = \x -> g (f x)
{-# INLINE (.>) #-}

-- helper function to make sure natVal calls are
-- zero runtime overhead
nv :: forall n . KnownNat n => Integer
nv = natVal' (proxy# :: Proxy# n)

-- convert an Integral number to Text
i2text :: Integral a => a -> Text
i2text = TextBuilder.decimal .> TextBuilder.toLazyText .> TextLazy.toStrict
{-# SPECIALISE i2text :: Int -> Text #-}
{-# SPECIALISE i2text :: Integer -> Text #-}

ref :: Predicate p x => x -> String -> Either [String] (Refined p x)
ref x tag = left (\e -> [tag <> ": " <> show e]) (refine x)
