{-# LANGUAGE OverloadedStrings #-}
module FFIDeclarations where

import qualified Data.Text.Format as TF

dataDeclaration :: TF.Format
dataDeclaration = "data {-# CLASS {} #-} {} = {} (Object# {}) deriving Class"
