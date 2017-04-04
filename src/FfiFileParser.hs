{-# LANGUAGE ScopedTypeVariables #-}
module FfiFileParser(parseFFI) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Map.Strict as M

type FFIInfo = (BL.ByteString, BL.ByteString)

parseFFI :: BL.ByteString -> Map BL.ByteString BL.ByteString
parseFFI csvData =
  case decode NoHeader csvData :: Either String (V.Vector FFIInfo) of
    Left _ -> M.empty
    Right v -> V.foldr (\ (x,y) m -> M.insert x y m) M.empty v
