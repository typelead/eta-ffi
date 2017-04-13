{-# LANGUAGE ScopedTypeVariables #-}
module FfiFileParser  where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Map.Strict as M
import Data.Text

type JavaClassName = Text
type EtaPackage = Text
type EtaModule = Text
type EtaType = Text


--type FFIInfo = (BL.ByteString, BL.ByteString)
type FFIInfo = (JavaClassName,EtaPackage,EtaModule,EtaType)

-- parseFFI :: BL.ByteString -> Map BL.ByteString BL.ByteString
-- parseFFI csvData =
--   case decode NoHeader csvData :: Either String (V.Vector FFIInfo) of
--     Left _ -> M.empty
--     Right v -> V.foldr (\ (x,y) m -> M.insert x y m) M.empty v

parseFFI :: BL.ByteString -> Map JavaClassName (EtaPackage,EtaModule,EtaType)
parseFFI csvData =
  case decode NoHeader csvData :: Either String (V.Vector FFIInfo) of
    Left _ -> M.empty
    Right v -> V.foldr (\ (x,y,z,w) m -> M.insert x (y,z,w) m) M.empty v
