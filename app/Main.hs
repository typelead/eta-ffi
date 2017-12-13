module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data FFIConfig =
  FFIConfig
  { ffiMappingFiles :: [FilePath]
  , ffiOutputDir :: FilePath
  , ffiClasspath :: [FilePath]
  } deriving Show

parseFFIConfig :: Parser FFIConfig
parseFFIConfig =
  FFIConfig <$> many (strOption (short 'i'
                        <> long "include-mapping"
                        <> metavar "FILEPATH"
                        <> help "FILEPATH should be a valid filepath in the CSV format described above.\nThe basename of the FILEPATH will be used to determine the package name and version that it originates from.")
                     )
            <*> strOption (short 'o'
                        <> long "output-dir"
                        <> metavar "FILEPATH"
                        <> help "FILEPATH should be the location of a directory in which to generate the output."
                          )
            <*> many (strOption (long "classpath"
                        <> metavar "CLASSPATH"
                        <> help "Same as `javac`. The list of all the jars which you want to import from.")
                     )

main :: IO ()
main = startApp =<< execParser opts
  where
    opts = info (parseFFIConfig <**> helper)
      ( fullDesc
     <> progDesc "Eta FFI generator FILEPATH"
     <> header "Generate FFI in Eta" )

startApp :: FFIConfig -> IO ()
startApp config = print config
