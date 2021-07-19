{-# LANGUAGE RankNTypes #-}

module Effects.Retry where

import           Control.Retry

class Retry m where
  retry :: forall a . RetryPolicyM m -> (RetryStatus -> m a) -> m a

instance Retry IO where
  retry = recoverAll
