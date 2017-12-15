module Eta.FFI.Types.Spec where

import Data.Text (Text)
import Eta.FFI.Types.Core

newtype FFISpec = FFISpec { ffiTargets :: [Target] }

type LiteralString = Text
type RegEx = Text
type JavaType = Text
type EtaFunctionName = Text

data Target = Target { targetFilter :: Filter
                     , targetActions :: [Action] }

data Filter =
    FRegEx RegEx
  | FAnd [Filter]
  | FOr  [Filter]
  | FNot Filter
  | FPrefix LiteralString
  | FSuffix LiteralString
  | FScoped LiteralString Filter
  | FAbstract Bool
  | FLength Int
  | FSignature [JavaType]
  | FStatic Bool
  | FType JavaType
  deriving Show


data Safety = Safe | Unsafe | Interruptible

data Purity = Pure | Impure

data Action = Action { actionFilter :: Filter
                     , actionConstructors :: ConstructorAction
                     , actionMethods :: MethodAction
                     , actionFields :: FieldAction
                     , actionModulePrefix :: ModuleName
                     , actionPure :: Purity
                     , actionWrapper :: () }

type ConstructorAction = MethodAction
type FieldAction = MethodAction

data MethodAction =
  MethodAction { maFilter :: Filter
               , maAs :: EtaFunctionName
               , maSafety :: Safety
               , maPure :: Purity }
