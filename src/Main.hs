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
import Codec.JVM.Types hiding (Super)
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

data FFIState = FFIState { ffiFile :: Map JavaClassName (EtaPackage,EtaModule,EtaType)
                         , typeBounds :: Maybe TypeBounds}

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
type TypeBounds = Text

getSimpleClassName :: Text -> Text
getSimpleClassName = replace "/" "." >>> breakOnEnd "." >>> snd

--store the type bounds in global state
-- TODO: Only handles simple cases like class <E extends Foo> not class <E extends <Foo extends ..>>>
singleTypeVariable :: TypeVariableDeclaration TypeVariable -> (Text, Text) -- (x,x<:Object)
singleTypeVariable (TypeVariableDeclaration m y) =
  let x = toLower m -- convert "T"" to "t"" in Eta side
  in case (y !! 0) of
       NotBounded -> (x,(x <> " <: Object"))
       Extends (SimpleReferenceParameter (IClassName clsName)) -> (x,(x <> " <: " <> (getSimpleClassName clsName)))
       Super (SimpleReferenceParameter (IClassName clsName)) -> (x,((getSimpleClassName clsName) <> " <: " <> x))

allTypeVariables :: TypeVariableDeclarations TypeVariable -> (Text,TypeBounds) -- ("x y z", "x <: Foo, y <: Bar...")
allTypeVariables alltvs = let parsedParameters = map singleTypeVariable alltvs
                              typeVariables = L.foldl' (\y (a,_) -> y <> a <> " ") "" >>> dropEnd 1 $ parsedParameters
                              typeBounds = L.foldl' (\y (_,b) -> y <> b <> ",") "" >>> dropEnd 1 $ parsedParameters
                           in (typeVariables,typeBounds)

foldExtends :: Text -> ClassParameter TypeVariable -> Text
foldExtends t (GenericReferenceParameter (IClassName x) _ _) = t <> (getSimpleClassName x) <> ","
foldExtends t (SimpleReferenceParameter (IClassName x)) = t <> (getSimpleClassName x) <> ","

inherits :: [ClassParameter TypeVariable] -> Text  -- things this class actually inherits
inherits extendTypes = L.foldl' foldExtends "" >>> dropEnd 1 $ extendTypes

generateDataDeclaration :: ClassName -> Info -> (DataDeclaration,SubtypeDeclaration,Maybe TypeBounds)
generateDataDeclaration className info =
  let fqClassName = replace "/" "." className
      clsname = getSimpleClassName className
      ASignature (ClassSig (ClassSignature x y)) = (classAttributes info) !! 0
      -- x :: [TypeVariableDeclaration a]
      -- y :: [ClassParameter a]
      (genericClsName,objectClassname,typeBounds)  = case x of
                                                       [] -> (clsname,clsname,Nothing)
                                                       _  -> let g = (allTypeVariables x)
                                                                 y = clsname <> " " <> (fst g)
                                                             in (y,"(" <> y <> ")",Just $ snd g)
  in (TF.format dataDeclaration (fqClassName,genericClsName,clsname,objectClassname),
      TF.format subtypeDeclaration (genericClsName,(inherits y)),
      typeBounds)

-------------------------------------------------------------------------------------------------------------------
--------------------------Method Declarations-------------------------------------------------------------------
-- method - import public and protected
{-
data MethodInfo = MethodInfo
  { mi_accessFlags :: Set AccessFlag
  , mi_name :: UName
  , mi_descriptor :: Desc
  , mi_attributes :: [Attr]}
  deriving Show

data Info = Info
  {  interfaces  :: [InterfaceName]
   , fieldInfos  :: [FieldInfo]
   , methodInfos :: [MethodInfo]
   , classAttributes :: [Attr]}
   deriving Show

-}
{-
data ReferenceParameter a
  = -- | ClassTypeSignature
    GenericReferenceParameter
      ObjectType                     -- ^ PackageSpecifier & SimpleClassTypeSignature
      [TypeParameter a]              -- ^ SimpleClassTypeSignature
      [ReferenceParameter a]         -- ^ ClassTypeSignatureSuffix
    -- | Non Generic ClassTypeSignature
  | SimpleReferenceParameter ObjectType -- Ljava/lang/String;
    -- | TypeVariableSignature
  | VariableReferenceParameter a
    -- | ArrayTypeSignature
  | ArrayReferenceParameter    (Parameter a)


-}

quux :: Parameter TypeVariable -> Text
quux (ReferenceParameter (GenericReferenceParameter x y z)) = undefined
quux (ReferenceParameter (SimpleReferenceParameter x)) = undefined
quux (ReferenceParameter (VariableReferenceParameter x)) = undefined
quux (PrimitiveParameter x) = case x of
                                JByte   -> "Byte"
                                JChar   -> "JChar"
                                JDouble -> "Double"
                                JFloat  -> "Float"
                                JInt    -> "Int"
                                JLong   -> "Int64"
                                JShort  -> "Short"
                                JBool   -> "Bool"

-- foreign import java unsafe "canExecute" canExecute1 :: Java File Bool
-- foreign import java unsafe toString :: (a <: Object) => a -> String
bar :: UName -> Attr -> Maybe TypeBounds -> TL.Text
bar (UName t) (ASignature (MethodSig (MethodSignature _ x y _))) typeBounds = undefined
-- x :: [MethodParameter TypeVariable] => [Parameter a]
-- y :: MethodReturn TypeVariable => Maybe (Parameter a) 

-- foreign import java unsafe "@static java.io.File.createTempFile" createTempFile  :: String -> String -> Java a File
foo :: MethodInfo -> Maybe TypeBounds -> Maybe Integer
foo MethodInfo {mi_accessFlags=accessFlags,mi_name=name,mi_descriptor=descriptor,mi_attributes=attributes} typeBounds =
  case (S.member Private accessFlags) of
       True -> Nothing
       False -> case (S.member Static accessFlags) of
                  True -> Just 5
                  False -> Just 5


generateMethodDeclaration :: ClassName -> Info -> Maybe TypeBounds -> Text
generateMethodDeclaration className info typeBounds =
  let mInfos = methodInfos info
    in  "hello"
------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------
---------------------------------------Field declarations-------------------------------------------------------
-- field - import public






-------------------------------------------------------------------------------------------------------------------
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
           map (runGet parseClassFile) >>> -- [(ClassName,Info)] 
           foldr (\ (a,b) m -> M.insert a b m) M.empty
      finalFFIMap = f2 fileContent -- (Map ClassName Info)

  ------------------------------------------------------------------
  --------------------Generating data declaration------------------

  -- generateDataDeclaration ClassName Info -> (data decls,inherits, typebounds)
  -- store typebounds in state

  -----------------------------------------------------------------

  --------------------Generate method declaration------------------


  

  -----------------------------------------------------------------

  --------------------Generate field declaration------------------


  

  -----------------------------------------------------------------
        -- reset the state
  -----------------------------------------------------------------



  liftIO $ writeFile "Types.hs" "abc"
  liftIO $ writeFile "Methods.hs" "abc"

      -- check flag for multiple file or single file
      -- Map Classname Info
     -- traverse the map or vector and run a function
      -- function :: Classname -> Info -> TL.Text
  return ()
