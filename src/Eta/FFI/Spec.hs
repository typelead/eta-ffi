module Eta.FFI.Spec where

import Eta.FFI.Types
import Eta.FFI.Filter
import Java
import Java.String
import Java.Collections

mkFFISpec :: [Target] -> FFISpec
mkFFISpec = FFISpec

mkTarget :: Filter -> [Action] -> Target
mkTarget filter actions =
  Target { targetFilter  = filter
         , targetActions = actions }

getFilteredClasses :: Classpath -> Filter -> Maybe [JavaClass]
getFilteredClasses classpath filter =
  fmap fromJava $ getClasses (toJava classpath') (toPredicate filter)
  where classpath' = map toJava classpath :: [JString]

foreign import java unsafe "@static eta.ffi.Utils.getClasses"
  getClasses :: JStringArray -> Predicate -> Maybe (Set JavaClass)

