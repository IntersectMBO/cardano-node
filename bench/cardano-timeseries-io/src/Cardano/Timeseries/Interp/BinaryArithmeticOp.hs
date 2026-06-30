module Cardano.Timeseries.Interp.BinaryArithmeticOp(BinaryArithmeticOp(..),
  embedScalar, embedInstantVectorScalar, embedRangeVectorScalar,
  mbBinaryArithmeticOpScalar, mbBinaryArithmeticOpInstantVectorScalar,
  mbBinaryArithmeticOpRangeVectorScalar, materializeScalar) where

import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Interp.Expr (Expr (..))

data BinaryArithmeticOp = Add | Sub | Mul | Div deriving (Show, Eq, Ord)

instance AsText BinaryArithmeticOp where
  asText = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"

embedScalar :: BinaryArithmeticOp -> Expr -> Expr -> Expr
embedScalar Add = AddScalar
embedScalar Sub = SubScalar
embedScalar Mul = MulScalar
embedScalar Div = DivScalar

embedInstantVectorScalar :: BinaryArithmeticOp -> Expr -> Expr -> Expr
embedInstantVectorScalar Add = AddInstantVectorScalar
embedInstantVectorScalar Sub = SubInstantVectorScalar
embedInstantVectorScalar Mul = MulInstantVectorScalar
embedInstantVectorScalar Div = DivInstantVectorScalar

embedRangeVectorScalar :: BinaryArithmeticOp -> Expr -> Expr -> Expr
embedRangeVectorScalar Add = AddRangeVectorScalar
embedRangeVectorScalar Sub = SubRangeVectorScalar
embedRangeVectorScalar Mul = MulRangeVectorScalar
embedRangeVectorScalar Div = DivRangeVectorScalar

mbBinaryArithmeticOpInstantVectorScalar :: Expr -> Maybe (Expr, BinaryArithmeticOp, Expr)
mbBinaryArithmeticOpInstantVectorScalar (AddInstantVectorScalar a b) = Just (a, Add, b)
mbBinaryArithmeticOpInstantVectorScalar (SubInstantVectorScalar a b) = Just (a, Sub, b)
mbBinaryArithmeticOpInstantVectorScalar (MulInstantVectorScalar a b) = Just (a, Mul, b)
mbBinaryArithmeticOpInstantVectorScalar (DivInstantVectorScalar a b) = Just (a, Div, b)
mbBinaryArithmeticOpInstantVectorScalar _ = Nothing

mbBinaryArithmeticOpRangeVectorScalar :: Expr -> Maybe (Expr, BinaryArithmeticOp, Expr)
mbBinaryArithmeticOpRangeVectorScalar (AddRangeVectorScalar a b) = Just (a, Add, b)
mbBinaryArithmeticOpRangeVectorScalar (SubRangeVectorScalar a b) = Just (a, Sub, b)
mbBinaryArithmeticOpRangeVectorScalar (MulRangeVectorScalar a b) = Just (a, Mul, b)
mbBinaryArithmeticOpRangeVectorScalar (DivRangeVectorScalar a b) = Just (a, Div, b)
mbBinaryArithmeticOpRangeVectorScalar _ = Nothing

mbBinaryArithmeticOpScalar :: Expr -> Maybe (Expr, BinaryArithmeticOp, Expr)
mbBinaryArithmeticOpScalar (AddScalar a b) = Just (a, Add, b)
mbBinaryArithmeticOpScalar (SubScalar a b) = Just (a, Sub, b)
mbBinaryArithmeticOpScalar (MulScalar a b) = Just (a, Mul, b)
mbBinaryArithmeticOpScalar (DivScalar a b) = Just (a, Div, b)
mbBinaryArithmeticOpScalar _ = Nothing

materializeScalar :: BinaryArithmeticOp -> Double -> Double -> Double
materializeScalar Add = (+)
materializeScalar Sub = (-)
materializeScalar Mul = (*)
materializeScalar Div = (/)
