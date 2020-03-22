module Utils.Lift where

import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           UnliftIO.Exception             ( Exception )

data EmptyMaybeException = EmptyMaybeException deriving Show

instance Exception EmptyMaybeException

liftMaybe :: MonadThrow m => Maybe a -> m a
liftMaybe (Just a) = pure a
liftMaybe Nothing  = throwM EmptyMaybeException
