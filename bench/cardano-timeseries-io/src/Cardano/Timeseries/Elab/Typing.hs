{-# LANGUAGE FlexibleInstances #-}

module Cardano.Timeseries.Elab.Typing(
  Ty(..),
  Binding(..),
  Context,
  identifier,
  Def(..),
  Defs,
  instantiateTy,
  instantiateExpr,
  prettyTy,
  TyPrec(..),
  ty
  ) where

import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Domain.Identifier (Identifier)
import           Cardano.Timeseries.Interp.Expr (HoleIdentifier)
import qualified Cardano.Timeseries.Interp.Expr as Semantic

import           Data.Foldable as Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text

-- | Typing of a query expression.
data Ty = InstantVector Ty
        | RangeVector Ty
        | Scalar
        | Text
        | Bool
        | Pair Ty Ty
        | Timestamp
        | Duration
        | Fun Ty Ty
        | Hole HoleIdentifier deriving (Show, Eq)

data TyPrec = Loose | FunCodomain | FunDomain | Tight deriving (Show, Eq, Ord)

conditionalParens :: Bool -> Text -> Text
conditionalParens True t  = "(" <> t <> ")"
conditionalParens False t = t

prettyTy :: TyPrec -> Ty -> Text
prettyTy prec (InstantVector typ) = conditionalParens (prec == Tight) $
  "InstantVector " <> prettyTy Tight typ
prettyTy prec (RangeVector typ) = conditionalParens (prec == Tight) $
  "RangeVector " <> prettyTy Tight typ
prettyTy _prec (Pair typ typ') =
  "(" <> prettyTy Loose typ <> ", " <> prettyTy Loose typ' <> ")"
prettyTy prec (Fun typ typ') = conditionalParens (prec > FunCodomain) $
  prettyTy FunDomain typ <> " -> " <> prettyTy FunCodomain typ'
prettyTy _ Scalar = "Scalar"
prettyTy _ Bool = "Bool"
prettyTy _ Timestamp = "Timestamp"
prettyTy _ Duration = "Duration"
prettyTy _ Text = "Text"
prettyTy _ (Hole idx) = "?" <> showT idx

-- | A context entry of a typing context.
data Binding = LetBinding Identifier Semantic.Expr Ty
             | LambdaBinding Identifier Ty deriving (Show)

instance AsText Binding where
  asText (LetBinding x _rhs typ) = "(" <> asText x <> " ≔ " <> "..." <> " : " <> prettyTy Loose typ <> ")"
  asText (LambdaBinding x typ)  = "(" <> asText x <> " : " <> prettyTy Loose typ <> ")"

identifier :: Binding -> Identifier
identifier (LetBinding x _ _)  = x
identifier (LambdaBinding x _) = x

ty :: Binding -> Ty
ty (LetBinding _ _ typ)  = typ
ty (LambdaBinding _ typ) = typ

-- | Γ
--   A typing context of a query expression.
type Context = Seq Binding

instance AsText Context where
  asText Empty = "()"
  asText ctx   = Text.unwords $ map asText $ Foldable.toList ctx

-- | (? type) | (? ≔ T type) | (? : T) | (? ≔ t : T)
--   Definition of a type- or expression- level hole.
data Def = TyHoleDecl | TyHoleInst Ty | ExprHoleDecl Ty | ExprHoleInst Semantic.Expr Ty

-- | Σ
--   A collection of hole definitions `Def` indexed by `HoleIdentifier`.
type Defs = Map HoleIdentifier Def

-- | Assumes that the given `Defs` contains a `TyHoleDecl` of the given `HoleIdentifier`.
instantiateTy :: HoleIdentifier -> Ty -> Defs -> Defs
instantiateTy x rhs defs =
  case Map.lookup x defs of
    Just TyHoleDecl -> Map.insert x (TyHoleInst rhs) defs
    _ -> error $ "[INTERNAL ERROR] Incorrect or missing Def for type hole identifier: " <> show x

-- | Assumes that the given `Defs` contains a `ExprHoleDecl` of the given `HoleIdentifier`.
--   Types of the hole and the provided `Semantic.Expr` must be compatible.
instantiateExpr :: HoleIdentifier -> Semantic.Expr -> Defs -> Defs
instantiateExpr x rhs defs =
  case Map.lookup x defs of
    Just (ExprHoleDecl typ) -> Map.insert x (ExprHoleInst rhs typ) defs
    _ -> error $ "[INTERNAL ERROR] Incorrect or missing Def for expr hole identifier: " <> show x
