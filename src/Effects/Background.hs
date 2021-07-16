module Effects.Background where

import           Control.Concurrent.Async       ( async )
import           Control.Concurrent             ( threadDelay )
import           Data.Functor                   ( void )
import           Refined

newtype Minutes = Mins (Refined Positive Int) deriving Show

{-
 - Schedules a process to run in the background after the
 - given period of time.
 -}
class Background m where
  schedule :: m a -> Minutes -> m ()

instance Background IO where
  schedule fa mins = void . async $ threadDelay (microseconds mins) >> fa
    where microseconds (Mins ms) = 60000000 * unrefine ms
