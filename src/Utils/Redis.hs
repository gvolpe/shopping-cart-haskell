{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Utils.Redis where

import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.UTF8     as B
import qualified Data.ByteString.Lazy          as B
import           Data.UUID                      ( UUID )
import qualified Data.UUID                     as UUID
import qualified Database.Redis                as R
import           Domain.Item
import           Text.Read                      ( readMaybe )
import           Utils.Text                     ( normalizeBS )
import           UnliftIO.Exception             ( Exception
                                                , fromEitherM
                                                )

instance Exception R.Reply

readInt :: forall n a . (Integral n, Read n) => (n -> a) -> C.ByteString -> Maybe a
readInt f x = f <$> (readMaybe . B.toString $ B.fromStrict x)

readUUID :: (UUID -> a) -> C.ByteString -> Maybe a
readUUID f x = f <$> UUID.fromText (normalizeBS x)

writeUUID :: UUID -> C.ByteString
writeUUID = C.pack . UUID.toString

runRedisM :: R.Connection -> R.Redis (Either R.Reply a) -> IO a
runRedisM conn cmd = fromEitherM $ R.runRedis conn cmd
