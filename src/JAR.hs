module JAR where

import Codec.Archive.Zip
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map.Lazy (keys)
import Path
import Path.IO (copyFile)
import Data.ByteString.Lazy
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


withUnsafePath filepath action1 action2 = undefined
