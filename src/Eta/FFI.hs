module Eta.FFI where

import Java
import Eta.FFI.Types
import Eta.FFI.Spec

generateFFI :: FFISpec -> Classpath -> FFIMapping
            -> Either FFIError [(ModuleName, Contents)]
generateFFI ffispec classpath ffimapping = undefined
  where targetClasses = getTargetClasses ffispec
