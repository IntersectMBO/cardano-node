module Cardano.Timeseries.Query.BinaryRelation(
  BinaryRelation(..),
  embedScalar,
  embedInstantVectorScalar,
  mbBinaryRelationInstantVector,
  mbBinaryRelationScalar,
  swapInstantVectorScalar,
  materializeScalar) where

import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Query.Expr (Expr (..))

-- | A datatype used to carve out a subset of `Function` that represents binary relations.
data BinaryRelation = Eq | Lt | Lte | Gt | Gte | NotEq deriving (Show, Eq, Ord)

instance AsText BinaryRelation where
  asText = \case
    Eq -> "=="
    Lt -> "<"
    Lte -> "<="
    Gt -> ">"
    Gte -> ">="
    NotEq -> "!="

embedScalar :: BinaryRelation -> Expr -> Expr -> Expr
embedScalar Eq    = EqScalar
embedScalar Lt    = LtScalar
embedScalar Lte   = LteScalar
embedScalar Gt    = GtScalar
embedScalar Gte   = GteScalar
embedScalar NotEq = NotEqScalar

embedInstantVectorScalar :: BinaryRelation -> Expr -> Expr -> Expr
embedInstantVectorScalar Eq    = EqInstantVectorScalar
embedInstantVectorScalar Lt    = LtInstantVectorScalar
embedInstantVectorScalar Lte   = LteInstantVectorScalar
embedInstantVectorScalar Gt    = GtInstantVectorScalar
embedInstantVectorScalar Gte   = GteInstantVectorScalar
embedInstantVectorScalar NotEq = NotEqInstantVectorScalar

-- k < v <=> v > k
-- k ≤ v <=> v ≥ k
-- k > v <=> v < k
-- k ≥ v <=> v ≤ k
-- k = v <=> v = k
-- k ≠ v <=> v ≠ k
swapInstantVectorScalar :: BinaryRelation -> BinaryRelation
swapInstantVectorScalar Eq    = Eq
swapInstantVectorScalar NotEq = NotEq
swapInstantVectorScalar Lt    = Gt
swapInstantVectorScalar Lte   = Gte
swapInstantVectorScalar Gt    = Lt
swapInstantVectorScalar Gte   = Lte

mbBinaryRelationInstantVector :: Expr -> Maybe (Expr, BinaryRelation, Expr)
mbBinaryRelationInstantVector (EqInstantVectorScalar a b)    = Just (a, Eq, b)
mbBinaryRelationInstantVector (LtInstantVectorScalar a b)    = Just (a, Lt, b)
mbBinaryRelationInstantVector (LteInstantVectorScalar a b)   = Just (a, Lte, b)
mbBinaryRelationInstantVector (GtInstantVectorScalar a b)    = Just (a, Gt, b)
mbBinaryRelationInstantVector (GteInstantVectorScalar a b)   = Just (a, Gte, b)
mbBinaryRelationInstantVector (NotEqInstantVectorScalar a b) = Just (a, NotEq, b)
mbBinaryRelationInstantVector _                              = Nothing

mbBinaryRelationScalar :: Expr -> Maybe (Expr, BinaryRelation, Expr)
mbBinaryRelationScalar (EqScalar a b)    = Just (a, Eq, b)
mbBinaryRelationScalar (LtScalar a b)    = Just (a, Lt, b)
mbBinaryRelationScalar (LteScalar a b)   = Just (a, Lte, b)
mbBinaryRelationScalar (GtScalar a b)    = Just (a, Gt, b)
mbBinaryRelationScalar (GteScalar a b)   = Just (a, Gte, b)
mbBinaryRelationScalar (NotEqScalar a b) = Just (a, NotEq, b)
mbBinaryRelationScalar _                 = Nothing

materializeScalar :: BinaryRelation -> Double -> Double -> Bool
materializeScalar Eq    = (==)
materializeScalar Lt    = (<)
materializeScalar Lte   = (<=)
materializeScalar Gt    = (>)
materializeScalar Gte   = (>=)
materializeScalar NotEq = (/=)
