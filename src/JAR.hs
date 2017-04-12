module JAR where

import Codec.Archive.Zip
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map.Lazy (keys)
import Path
import Path.IO (copyFile)
import Data.ByteString.Internal
import System.Directory hiding (copyFile)

getFilesFromJar :: (MonadThrow m, MonadCatch m, MonadIO m) =>
                   FilePath -> m [(Path Rel File, ByteString)]
getFilesFromJar jarLocation =
  withUnsafePath jarLocation (flip withArchive action) (flip withArchive action)
  where action = do
          entrySelectors <- keys <$> getEntries
          forM entrySelectors $ \es -> do
            contents <- getEntry es
            return (unEntrySelector es, contents)

withUnsafePath :: (MonadThrow m) => FilePath -> (Path Rel File -> m [(Path Rel File, ByteString)]) -> (Path Abs File -> m [(Path Rel File, ByteString)]) -> m [(Path Rel File, ByteString)]
withUnsafePath filepath action1 action2 =
  catch (do
            x <- parseRelFile filepath
            y <- action1 x
            return y)
         (\_ -> do
             x <- parseAbsFile filepath
             y <- action2 x
             return y)

