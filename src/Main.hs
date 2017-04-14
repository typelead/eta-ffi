{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language PartialTypeSignatures #-}
module Main where

import Options.Applicative
import FfiFileParser
import Control.Category hiding ((.))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict as M hiding (map,filter,foldr)
import Data.Text hiding (map,filter,foldr)
import Data.Set as S hiding (map,filter,foldr)
import Path
import FFIDeclarations
import qualified Data.List as L
import qualified Data.Text.Internal.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Format as TF
--import Data.Functor.Identity
--import Data.Semigroup ((<>))

import Codec.JVM.Parse
import Codec.JVM.Attr
import Data.Binary.Get
import JAR (getFilesFromJar)

data FFIMapping = FFIMapping [FilePath]

data Application = Application
  { classpath :: String
  , ffi    :: FFIMapping
  , target :: String
  , packagePrefix :: String
  , globalFile :: Bool
  }

ffiMapping :: Parser FFIMapping
ffiMapping = FFIMapping <$> some (strOption
             (  long "ffi"
               <> short 'f'
               <> metavar "FILENAME"
               <> help "FFI file name" ))

application :: Parser Application
application = Application
              <$> strOption
              ( long "classpath"
                <> metavar "CLASSPATH"
                <> help "Target for the greeting" )
              <*> ffiMapping
              <*> strOption
              ( long "target"
                <> metavar "TARGET"
                <> help "Target version" )
              <*> strOption
              ( long "package-prefix"
                <> short 'p'
                <> help "The prefix you want for your packages" )
              <*> switch
              ( long "global-multi-file"
                <> help "Individual Eta module per class" )

readFiles :: [FilePath] -> IO BL.ByteString
readFiles = fmap BL.concat . mapM BL.readFile
------------------------------------------------------------------------------
main :: IO ()
main = app =<< execParser opts
  where
    opts = info (application <**> helper)
      (header "a test for optparse-applicative" )

app :: Application -> IO ()
app = undefined
-- app (Application {ffi = FFIMapping filepaths,..}) = do
--   csvDatas <- readFiles filepaths
--   evalStateT ffiAction (FfiState {ffiFile = parseFFI csvDatas})

data FFIState = FFIState { ffiFile :: Map JavaClassName (EtaPackage,EtaModule,EtaType)}

type Env = M.Map String String -- some Environment

type FFIMonad a = ReaderT Env (ExceptT String (StateT FFIState IO)) a

runFFI :: Env -> FFIState -> FFIMonad a -> IO ((Either String a),FFIState)
runFFI env st m = (runStateT (runExceptT (runReaderT m env)) st)

parsePackageName :: String -> Text
parsePackageName globPattern =
  let textGlobPattern = pack globPattern
      x = pack "."
      y = pack "/"
  in case find (\c -> c == '*') textGlobPattern of
       Just _ -> dropEnd 1 textGlobPattern
       Nothing -> replace x y textGlobPattern

-- TODO ::Check inside [InterfaceName] too
filterFFItoGenerate :: [(FilePath, (ClassName,SuperClassName,[InterfaceName]))] -> Map JavaClassName (EtaPackage,EtaModule,EtaType) -> Set FilePath
filterFFItoGenerate infos ffiFile =
  (filter (\(_,(a,b,_)) -> ((M.lookup a ffiFile) == Nothing) &&
                           ((M.lookup b ffiFile) == Nothing)) >>>
    map fst >>>
    S.fromList) infos

type DataDeclaration = TL.Text
type SubtypeDeclaration = TL.Text

--store the type bounds in global state
singleTypeParameter :: TypeParameter -> (Text, Maybe Text) -- (x,x<:Object)
singleTypeParameter tp = case tp of
                           TPSimpleTypeVariable x -> (x,Nothing)
                           TPExtendsClass x (JReferenceType (SimpleClassName y)) -> (x, Just $ x <> " <: " <> y)
                           TPSuperClass x (JReferenceType (SimpleClassName y)) -> (x, Just $ y <> " <: " <> x)

allTypeParameters :: [TypeParameter] -> (Text,Text) -- ("x y z", "x <: Foo, y <: Bar...")
allTypeParameters alltps = let parsedParameters = map singleTypeParameter alltps
                               typeVariables = L.foldl' (\y (a,_) -> y <> a) "" parsedParameters
                               typeBounds = L.foldl' (\y (_,b) -> case b of
                                                      Just s -> y <> s <> ",") "" >>> dropEnd 1 $ parsedParameters
                           in (typeVariables,typeBounds)

inherits :: [MReturnType] -> Text  -- things this class actually inherits
inherits extendTypes = L.foldl' (\ t (JReferenceType (SimpleClassName x)) -> t <> x <> ",") "" >>> dropEnd 1 $ extendTypes

generateDataDeclaration :: ClassName -> Info -> (DataDeclaration,SubtypeDeclaration)
generateDataDeclaration className info =
  let fqClassName = replace "/" "." className
      className = snd $ breakOnEnd "." fqClassName
      ASignature (SigCSignature (CSignature x y)) = (classAttributes info) !! 0
      -- x :: Maybe [TypeParameter]
      -- y :: [Extends:: MReturnType]
  in (TF.format dataDeclaration (fqClassName,className,className,className),
      TF.format dataDeclaration (fqClassName,className,className,className))
-- 2nd and 4th argument should be generic

codeGenerator :: ClassName -> Info -> TL.Text
codeGenerator className classInfo = undefined

ffiAction :: FFIMonad ()
ffiAction = do
  env <- ask
  -- fileContent :: [(Path Rel File, ByteString)]
  fileContent <- case M.lookup "filepath" env of
                   Nothing -> throwError "Filepath not defined"
                   Just path -> getFilesFromJar path
  package <-  case M.lookup "package-name" env of
                Nothing -> throwError "package name not provided"
                Just packageName -> return $ parsePackageName packageName
  FFIState {ffiFile = file}  <- get
  let f = map (\(a,b) -> (toFilePath a,b)) >>>
          filter (\(a,_) -> package == (pack a)) >>>
          map (\(a,b) -> (a,runGet parseClassFileHeaders b))
      -- ffiToGenerate :: Set FilePath
      ffiToGenerate = filterFFItoGenerate (f fileContent) file
      f2 = map (\(a,b) -> (toFilePath a,b)) >>>
           filter (\(a,_) -> S.member a ffiToGenerate) >>>
           map snd >>>
           map (runGet parseClassFile) >>>
           foldr (\ (a,b) m -> M.insert a b m) M.empty
      finalFFIMap = f2 fileContent

  ------------------------------------------------------------------
  --------------------Generating data declaration------------------

  

  -----------------------------------------------------------------


  liftIO $ writeFile "Types.hs" "abc"
  liftIO $ writeFile "Methods.hs" "abc"

      -- check flag for multiple file or single file
      -- Map Classname Info
     -- traverse the map or vector and run a function
      -- function :: Classname -> Info -> TL.Text
  return ()
