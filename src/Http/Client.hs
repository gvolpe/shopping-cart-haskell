{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Http.Client
  ( HTTPClient(..)
  )
where

import           Control.Lens
import           Data.Aeson                     ( FromJSON )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Network.Wreq
import           Network.Wreq.Types             ( Postable )

class HTTPClient m where
  get :: forall a . FromJSON a => Options -> Text -> m a
  post :: forall a b . (FromJSON a, Postable b) => Options -> Text -> b -> m a

instance HTTPClient IO where
  get  = mkGet
  post = mkPost

mkGet :: forall a . FromJSON a => Options -> Text -> IO a
mkGet ops url =
  (^. responseBody)
    <$> (asJSON =<< getWith ops (T.unpack url) :: IO (Response a))

mkPost :: forall a b . (FromJSON a, Postable b) => Options -> Text -> b -> IO a
mkPost ops url body =
  (^. responseBody)
    <$> (asJSON =<< postWith ops (T.unpack url) body :: IO (Response a))
