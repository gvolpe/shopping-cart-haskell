module Effects.Logger where

import           Control.Monad.IO.Class         ( liftIO )
import           Colog.Core.Action              ( (<&) )
import           Colog.Core.IO                  ( logStringStdout
                                                , logStringStderr
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Servant

class Logger m where
  logInfo :: Text -> m ()
  logError :: Text -> m ()

instance Logger IO where
  logInfo  = liftIO . (logStringStdout <&) . unpack
  logError = liftIO . (logStringStderr <&) . unpack

instance Logger Handler where
  logInfo  = liftIO . (logStringStdout <&) . unpack
  logError = liftIO . (logStringStderr <&) . unpack
