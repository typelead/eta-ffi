module Main where

import Options.Applicative

--import Data.Semigroup ((<>))

{-
eta-ffi -jar spark-2.0.1.jar org.apache.spark.* -package-prefix Spark
Flags:
-classplath (same format as javac)
-ffi (accepts .ffi files which contain a 
  - mapping from Java class/interface/enum to an Eta type, Eta module, and Eta package
-target (which Eta version to target)
-package-prefix
--global-single-file (Default) Means all methods for a given package should be in a single file called Methods.hs
--single-file [package]
--multi-file [package]
---global-multi-file (Manual) Means individual Eta module per Java class
[input-package] support glob patterns (ex: java.util.*) (ex: java.util.**)
Single * means just that package
** means nested packages as well.

-}

data FFIMapping = FFIMapping FilePath

data Application = Application
  { classpath :: String
  , ffi    :: FFIMapping
  , target :: String
  , packagePrefix :: String
  , globalFile :: Bool
  }

ffiMapping :: Parser FFIMapping
ffiMapping = FFIMapping <$> strOption
             (  long "ffi"
               <> short 'f'
               <> metavar "FILENAME"
               <> help "FFI file name" )

application :: Parser Application
application = Application
              <$> strOption
              ( long "classpath"
                <> metavar "TARGET"
                <> help "Target for the greeting" )
              <*> ffiMapping
              <*> strOption
              ( long "hello"
                <> metavar "TARGET"
                <> help "Target for the greeting" )
              <*> strOption
              ( long "quiet"
                <> short 'q'
                <> help "Whether to be quiet" )
              <*> switch
              ( long "global-multi-file"
                <> help "Individual Eta module per class" )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (application <**> helper)
      (header "hello - a test for optparse-applicative" )

greet :: Application -> IO ()
greet (Application c f t p False) = putStrLn $ "Test" ++c ++ t ++ p
greet (Application c f t p True) = putStrLn $ "Test" ++c ++ t ++ p 

