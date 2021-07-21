{-# LANGUAGE DeriveAnyClass, DerivingStrategies, LambdaCase, RankNTypes #-}

module Utils.Errors where

import           Control.Monad.Catch
import           Data.List.NonEmpty

data EmptyMaybeException = EmptyMaybeException
  deriving stock Show
  deriving anyclass Exception

attempt :: forall m a . MonadCatch m => m a -> m (Either SomeException a)
attempt = try

ensureNonEmpty
  :: forall m a e . (Exception e, MonadThrow m) => e -> [a] -> m (NonEmpty a)
ensureNonEmpty e xs = case nonEmpty xs of
  Nothing   -> throwM e
  (Just ys) -> pure ys

fromEitherM :: Exception e => IO (Either e a) -> IO a
fromEitherM fa = fa >>= \case
  Right a -> pure a
  Left  e -> throwM e

liftMaybe :: MonadThrow m => Maybe a -> m a
liftMaybe (Just a) = pure a
liftMaybe Nothing  = throwM EmptyMaybeException
