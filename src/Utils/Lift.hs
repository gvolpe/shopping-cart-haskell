{-# LANGUAGE LambdaCase #-}

module Utils.Lift where

import           Control.Monad.Catch            ( Exception
                                                , MonadThrow
                                                , throwM
                                                )

data EmptyMaybeException = EmptyMaybeException deriving Show

instance Exception EmptyMaybeException

fromEitherM :: Exception e => IO (Either e a) -> IO a
fromEitherM fa = fa >>= \case
  Right a -> pure a
  Left  e -> throwM e

liftMaybe :: MonadThrow m => Maybe a -> m a
liftMaybe (Just a) = pure a
liftMaybe Nothing  = throwM EmptyMaybeException
