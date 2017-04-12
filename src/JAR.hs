{-# LANGUAGE ScopedTypeVariables #-}
module JAR where

import Codec.Archive.Zip
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map.Lazy (keys)
import Path
import Data.ByteString.Internal


getFilesFromJar :: (MonadThrow m, MonadCatch m, MonadIO m) =>
                   FilePath -> m [(Path Rel File, ByteString)]
getFilesFromJar jarLocation =
  withUnsafePath jarLocation (flip withArchive action) (flip withArchive action)
  where action = do
          entrySelectors <- keys <$> getEntries
          forM entrySelectors $ \es -> do
            contents <- getEntry es
            return (unEntrySelector es, contents)

withUnsafePath :: (MonadCatch m, MonadThrow m) => FilePath -> (Path Rel File -> m [(Path Rel File, ByteString)]) -> (Path Abs File -> m [(Path Rel File, ByteString)]) -> m [(Path Rel File, ByteString)]
withUnsafePath filepath action1 action2 =
  catch (parseRelFile filepath >>= action1)
        (\(_ :: PathParseException) -> parseAbsFile filepath >>= action2)
