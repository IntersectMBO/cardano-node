module Cardano.Timeseries.Query.BinaryRelation(
  BinaryRelation(..),
  embedScalar,
  mbBinaryRelationInstantVector,
  mbBinaryRelationScalar,
  materializeScalar) where
import           Cardano.Timeseries.Query.Expr (Function (..))

-- | A datatype used to carve out a subset of `Function` that represents binary relations.
data BinaryRelation = Eq | Lt | Lte | Gt | Gte | NotEq

embedScalar :: BinaryRelation -> Function
embedScalar Eq    = EqScalar
embedScalar Lt    = LtScalar
embedScalar Lte   = LteScalar
embedScalar Gt    = GtScalar
embedScalar Gte   = GteScalar
embedScalar NotEq = NotEqScalar

mbBinaryRelationInstantVector :: Function -> Maybe BinaryRelation
mbBinaryRelationInstantVector EqInstantVectorScalar    = Just Eq
mbBinaryRelationInstantVector LtInstantVectorScalar    = Just Lt
mbBinaryRelationInstantVector LteInstantVectorScalar   = Just Lte
mbBinaryRelationInstantVector GtInstantVectorScalar    = Just Gt
mbBinaryRelationInstantVector GteInstantVectorScalar   = Just Gte
mbBinaryRelationInstantVector NotEqInstantVectorScalar = Just NotEq
mbBinaryRelationInstantVector _                        = Nothing

mbBinaryRelationScalar :: Function -> Maybe BinaryRelation
mbBinaryRelationScalar EqScalar    = Just Eq
mbBinaryRelationScalar LtScalar    = Just Lt
mbBinaryRelationScalar LteScalar   = Just Lte
mbBinaryRelationScalar GtScalar    = Just Gt
mbBinaryRelationScalar GteScalar   = Just Gte
mbBinaryRelationScalar NotEqScalar = Just NotEq
mbBinaryRelationScalar _           = Nothing

materializeScalar :: BinaryRelation -> Double -> Double -> Bool
materializeScalar Eq    = (==)
materializeScalar Lt    = (<)
materializeScalar Lte   = (<=)
materializeScalar Gt    = (>)
materializeScalar Gte   = (>=)
materializeScalar NotEq = (/=)
