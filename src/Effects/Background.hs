{-# LANGUAGE RecordWildCards #-}

module Effects.Background where

import           Data.Functor                   ( void )
import           Refined
import           UnliftIO.Async                 ( async )
import           UnliftIO.Concurrent            ( forkIO
                                                , threadDelay
                                                )

newtype Minutes = Mins {
  unMins :: Refined Positive Int
} deriving Show

{-
 - Schedules a process to run in the background after the
 - given period of time.
 -}
class Background m where
  schedule :: m a -> Minutes -> m ()

instance Background IO where
  schedule fa mins = void $ async (threadDelay (microseconds mins) >> fa)
    where microseconds Mins {..} = 60000000 * (unrefine unMins)
