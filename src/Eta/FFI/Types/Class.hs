module Eta.FFI.Types.Class where

import Java

data GenericDeclaration = GenericDeclaration @java.lang.reflect.GenericDeclaration
  deriving Class

data TypeVariable = TypeVariable @java.lang.reflect.TypeVariable
  deriving Class

data TypeVariableArray = TypeVariableArray @java.lang.reflect.TypeVariable[]
  deriving Class

instance JArray TypeVariable TypeVariableArray where

foreign import java unsafe getTypeParameters ::
  (a <: GenericDeclaration) => a -> TypeVariableArray

data Type = Type @java.lang.reflect.Type
  deriving Class

data TypeArray = TypeArray @java.lang.reflect.Type[]
  deriving Class

instance JArray Type TypeArray where

foreign import java unsafe getBounds :: TypeVariable -> TypeArray

foreign import java unsafe "getName" getTypeName :: TypeVariable -> String

data JavaClass = JavaClass @java.lang.Class
  deriving (Class, Show)

type instance Inherits JavaClass = '[Object, GenericDeclaration]

foreign import java unsafe getSimpleName :: JavaClass -> JString

foreign import java unsafe "getName" getClassName :: JavaClass -> JString

foreign import java unsafe getCanonicalName :: JavaClass -> Maybe JString

foreign import java unsafe getModifiers :: JavaClass -> Modifier

newtype Modifier = Modifier Int

foreign import java unsafe "@static java.lang.reflect.Modifier.isAbstract"
  isAbstract :: Modifier -> Bool

foreign import java unsafe "@static java.lang.reflect.Modifier.isInterface"
  isInterface :: Modifier -> Bool

foreign import java unsafe "@static java.lang.reflect.Modifier.isPublic"
  isPublic :: Modifier -> Bool

