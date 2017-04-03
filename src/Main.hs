module Main where

import Options.Applicative
import Data.Semigroup ((<>))

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

data Application = Application
  { classpath :: String
  , ffi    :: String
  , target :: String
  , packagePrefix :: String
  }


main :: IO ()
main = do
  putStrLn "hello world"
