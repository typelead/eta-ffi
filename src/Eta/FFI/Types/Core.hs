{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Eta.FFI.Types.Core where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Map (Map)
import Control.Monad.Except (Except, runExcept)

type FQCN = Text
type PackageName = Text
type ModuleName = Text
type TypeName = Text
type Contents  = Builder
type TypeContents  = Builder
type ModuleContents  = Builder
type Classpath = [FilePath]

-- Associates a fully-qualified class name (FQCN) to
-- a module and a type.
-- Example:
-- "java.util.Set" -> ("Java.Imports", "Set")
newtype FFIMapping = FFIMapping (Map FQCN (ModuleName, TypeName))


data FFIError
 = -- This error occurs when an FQCN
   -- is not found in the FFIMapping
   -- map.
   MissingFQCNMapping FQCN
 | -- This error occurs when an FQCN
   -- has mutliple Eta types that
   -- wrap it.
   DuplicateFQCNMappings
     [(PackageName, ModuleName, TypeName)]
 deriving Show

newtype FFI a = FFI (Except FFIError a)
  deriving (Functor, Applicative, Monad)

runFFI :: FFI a -> Either FFIError a
runFFI (FFI ex) = runExcept ex
