module Eta.FFI
  (module Eta.FFI
  ,module Eta.FFI.Spec
  )
where

import Java
import Eta.FFI.Types
import Eta.FFI.Spec
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import Data.Monoid
import Data.Maybe
import Data.Char
import Control.Monad
import qualified Data.Map as M

generateFFI :: FFISpec -> Classpath -> FFIMapping
            -> Either FFIError (M.Map ModuleName Contents)
generateFFI ffispec classpath ffimapping =
  runFFI $ fmap (M.unionsWith (<>)) $ mapM (generateTarget classpath ffimapping) $ ffiTargets ffispec

generateTarget :: Classpath -> FFIMapping -> Target -> FFI (M.Map ModuleName Contents)
generateTarget classpath ffimapping target = do
  maps <- forM filteredTargetClasses $ \(hasAbstractMethod, cls) -> do
       (modName, typeContents, _moduleContents)
         <- generateJavaClass ffimapping hasAbstractMethod cls
       let typesModName = "Interop." <> T.dropWhileEnd (/= '.') modName <> "Types"
       return $ M.singleton typesModName typeContents
  let combinedFragments = M.unionsWith (\l r -> l <> "\n" <> r) maps
      finalizeModules = M.mapWithKey (\k v ->
                                        "module " <> fromText k <> " where\n"
                                     <> "\nimport Java\n\n" <> v) combinedFragments
  return finalizeModules
  where allTargetClasses = getFilteredClasses classpath $ targetFilter target
        filteredTargetClasses = mapMaybe filterPublic $ fromMaybe [] allTargetClasses
        filterPublic c
          | isPublic m
          = Just (isInterface m || isAbstract m, c)
          | otherwise = Nothing
          where m = getModifiers c

{-
Say the class is org.bouncastle.crypto.Digest
This function should return

("Org.Bouncycastle.Crypto.Digest"
, "data Digest ..."
, "foreign import java unsafe..."
)
-}
generateJavaClass :: FFIMapping -> Bool
                  -> JavaClass -> FFI (ModuleName, TypeContents, ModuleContents)
generateJavaClass _ffiMapping _hasAbstractMethod cls
  = return (moduleName cls, typeContents cls, mempty)

-- getCanonicalName() returns . instead of $ for inner classes.
moduleName :: JavaClass -> ModuleName
moduleName = T.intersperse '.'
           . T.concat
           . map transform
           . T.split (== '.')
           . fromJava
           . getSomeName
  where transform t
          | Just (c, t') <- T.uncons t
          = T.cons (toUpper c) t'
          | otherwise = t
        getSomeName cls
          | Just name <- getCanonicalName cls
          = name
          | otherwise = getClassName cls

-- data List a = List (@java.util.List a)
-- data Map k v = Map (@java.util.Map k v)
typeContents :: JavaClass -> TypeContents
typeContents cls = "data " <> simpleName <> tvsText <> " = "
                           <> simpleName <> "(@" <> fcqn <> ")" <>
                 "\n  deriving Class\n"
  where tvs = fromJava (getTypeParameters cls)
        tvsText = mconcat $
          map ((singleton ' ' <>)
               . fromString
               . map toLower
               . getTypeName) tvs
        simpleName = fromText $ fromJava $ getSimpleName cls
        fcqn = fromText $ fromJava $ getClassName cls
