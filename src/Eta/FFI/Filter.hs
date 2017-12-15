module Eta.FFI.Filter
  (Filter
  ,toPredicate
  ,regex
  ,Predicate(..)
  )
where

import Java
import Java.Array
import Eta.FFI.Types
import qualified Data.Text as T
import Prelude hiding (and, or, not)

regex :: String -> Filter
regex ex = FRegEx (T.pack ex)

toPredicate :: Filter -> Predicate
toPredicate filter =
  case filter of
    FRegEx regex -> regexPredicate (toJava regex)
    FAnd filters -> andPredicate (toJava (map toPredicate filters))
    FOr filters  -> orPredicate (toJava (map toPredicate filters))
    FNot filter -> notPredicate (toPredicate filter)
    FPrefix literalString -> regexPredicate (toJava (T.cons '^' literalString))
    FSuffix literalString -> regexPredicate (toJava (T.snoc literalString '$'))
    f -> error $ "toPredicate: Not implemented yet: " ++ show f

foreign import java unsafe "@static com.google.common.base.Predicates.containsPattern"
  regexPredicate :: JString -> Predicate

foreign import java unsafe "@static com.google.common.base.Predicates.and"
  andPredicate :: PredicateArray -> Predicate

foreign import java unsafe "@static com.google.common.base.Predicates.or"
  orPredicate :: PredicateArray -> Predicate

foreign import java unsafe "@static com.google.common.base.Predicates.not"
  notPredicate :: Predicate -> Predicate

data Predicate = Predicate @com.google.common.base.Predicate
  deriving Class

data PredicateArray = PredicateArray @com.google.common.base.Predicate[]
  deriving Class

instance JArray Predicate PredicateArray where


