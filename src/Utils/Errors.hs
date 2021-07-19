{-# LANGUAGE LambdaCase, RankNTypes #-}

module Utils.Errors where

import           Control.Monad.Catch
import           Data.List.NonEmpty

attempt :: forall m a . MonadMask m => m a -> m (Either SomeException a)
attempt = try

ensureNonEmpty
  :: forall m a e . (Exception e, MonadThrow m) => e -> [a] -> m (NonEmpty a)
ensureNonEmpty e xs = case nonEmpty xs of
  Nothing   -> throwM e
  (Just ys) -> pure ys
