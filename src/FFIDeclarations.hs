{-# LANGUAGE OverloadedStrings #-}
module FFIDeclarations where

import qualified Data.Text.Format as TF

moduleDeclaration :: TF.Format
moduleDeclaration = "{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies #-} \n module {}.{} where \n import Java"

dataDeclaration :: TF.Format
dataDeclaration = "data {-# CLASS {} #-} {} = {} (Object# {}) deriving Class"

subtypeDeclaration :: TF.Format
subtypeDeclaration = "type instance Inherits {} = '[{}]"

instanceMethodDeclaration :: TF.Format
instanceMethodDeclaration = "foreign import java unsafe \"{}\" {} :: {}"

instanceFieldDeclaration :: TF.Format
instanceFieldDeclaration = "foreign import java unsafe \"@field {}\" {}  :: {}"

staticMethodDeclaration :: TF.Format
staticMethodDeclaration = "foreign import java unsafe \" @static {}\" {} :: {}"

staticFieldDeclaration :: TF.Format
staticFieldDeclaration = "foreign import java unsafe \"@static @field {}\" {} :: {}"

constructorDeclaration :: TF.Format
constructorDeclaration = "foreign import java unsafe \"@new\" {}  :: {}"
