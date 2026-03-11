module Cardano.Timeseries.Resolve(resolveTy, resolveBinding, resolveContext, resolveExpr') where
import           Cardano.Timeseries.Query.Expr (Expr (..))
import qualified Cardano.Timeseries.Query.Expr as Expr
import           Cardano.Timeseries.Typing
import qualified Cardano.Timeseries.Typing as Ty

import qualified Data.Map.Strict as Map


-- | Computes the head-normal form of `Ty` w.r.t. hole resolution (i.e. unfolds holes recursively up to the head expression).
resolveTy :: Defs -> Ty -> Ty
resolveTy defs (Ty.Hole x) =
  case Map.lookup x defs of
    Just (TyHoleInst rhs) -> resolveTy defs rhs
    Just _  -> Ty.Hole x
    Nothing -> error $ "[INTERNAL ERROR] Can't find hole in Σ: " <> show x
resolveTy defs (InstantVector typ) = InstantVector (resolveTy defs typ)
resolveTy defs (RangeVector typ) = RangeVector (resolveTy defs typ)
resolveTy defs (Fun typ typ') = Fun (resolveTy defs typ) (resolveTy defs typ')
resolveTy defs (Pair typ typ') = Pair (resolveTy defs typ) (resolveTy defs typ')
resolveTy _ Scalar = Scalar
resolveTy _ Timestamp = Timestamp
resolveTy _ Duration = Duration
resolveTy _ Bool = Bool
resolveTy _ Text = Text

-- | Computes the head-normal form of `Binding` w.r.t. hole resolution
--   (i.e. unfolds holes recursively up to the head expression in type of the binding).
resolveBinding :: Defs -> Binding -> Binding
resolveBinding defs (LetBinding x rhs typ) =
  LetBinding x rhs (resolveTy defs typ)
resolveBinding defs (LambdaBinding x typ) =
  LambdaBinding x (resolveTy defs typ)

-- | Computes the head-normal form of `Context` w.r.t. hole resolution
--   (i.e. unfolds holes recursively up to the head expression in every type of the context).
resolveContext :: Defs -> Context -> Context
resolveContext defs = fmap (resolveBinding defs)


-- | Computes the normal form of `Expr` w.r.t. hole resolution (i.e. resolves *all* holes in the expression).
resolveExpr' :: Defs -> Expr -> Expr
resolveExpr' _ Metrics = Metrics
resolveExpr' _ (Number f) = Number f
resolveExpr' _ (Str s) = Str s
resolveExpr' defs (AddInstantVectorScalar a b) = AddInstantVectorScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (SubInstantVectorScalar a b) = SubInstantVectorScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (MulInstantVectorScalar a b) = MulInstantVectorScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (DivInstantVectorScalar a b) = DivInstantVectorScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (AddScalar a b) = AddScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (SubScalar a b) = SubScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (MulScalar a b) = MulScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (DivScalar a b) = DivScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (EqScalar a b) = EqScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (NotEqScalar a b) = NotEqScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (LtScalar a b) = LtScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (LteScalar a b) = LteScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (GtScalar a b) = GtScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (GteScalar a b) = GteScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (EqInstantVectorScalar a b) = EqInstantVectorScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (NotEqInstantVectorScalar a b) = NotEqInstantVectorScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (LtInstantVectorScalar a b) = LtInstantVectorScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (LteInstantVectorScalar a b) = LteInstantVectorScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (GtInstantVectorScalar a b) = GtInstantVectorScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (GteInstantVectorScalar a b) = GteInstantVectorScalar (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' _ Expr.True = Expr.True
resolveExpr' _ Expr.False = Expr.False
resolveExpr' defs (Not t) = Not (resolveExpr' defs t)
resolveExpr' defs (And a b) = And (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (Or a b) = Or (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (EqBool a b) = EqBool (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (NotEqBool a b) = NotEqBool (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (Application f e) = Application (resolveExpr' defs f) (resolveExpr' defs e)
resolveExpr' defs (Lambda x f) = Lambda x (resolveExpr' defs f)
resolveExpr' defs (Let x rhs e) = Let x (resolveExpr' defs rhs) (resolveExpr' defs e)
resolveExpr' defs (MkPair a b) = MkPair (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' _ (Variable x) = Variable x
resolveExpr' _ (Milliseconds l) = Milliseconds l
resolveExpr' _ (Seconds l) = Seconds l
resolveExpr' _ (Minutes l) = Minutes l
resolveExpr' _ (Hours l) = Hours l
resolveExpr' defs (AddDuration a b) = AddDuration (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (Rewind a b) = Rewind (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (FastForward a b) = FastForward (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' _ Epoch = Epoch
resolveExpr' _ Now = Now
resolveExpr' defs (DurationToScalar t) = DurationToScalar (resolveExpr' defs t)
resolveExpr' defs (TimestampToScalar t) = TimestampToScalar (resolveExpr' defs t)
resolveExpr' defs (BoolToScalar t) = BoolToScalar (resolveExpr' defs t)
resolveExpr' defs (InstantVectorToScalar t) = InstantVectorToScalar (resolveExpr' defs t)
resolveExpr' defs (Abs t) = Abs (resolveExpr' defs t)
resolveExpr' defs (RoundScalar t) = RoundScalar (resolveExpr' defs t)
resolveExpr' defs (Fst t) = Fst (resolveExpr' defs t)
resolveExpr' defs (Snd t) = Snd (resolveExpr' defs t)
resolveExpr' defs (AvgOverTime t) = AvgOverTime (resolveExpr' defs t)
resolveExpr' defs (SumOverTime t) = SumOverTime (resolveExpr' defs t)
resolveExpr' defs (Avg t) = Avg (resolveExpr' defs t)
resolveExpr' defs (Min t) = Min (resolveExpr' defs t)
resolveExpr' defs (Max t) = Max (resolveExpr' defs t)
resolveExpr' defs (Rate t) = Rate (resolveExpr' defs t)
resolveExpr' defs (Increase t) = Increase (resolveExpr' defs t)
resolveExpr' defs (QuantileOverTime a b) = QuantileOverTime (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (Filter a b) = Filter (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (FilterByLabel a b) = FilterByLabel a (resolveExpr' defs b)
resolveExpr' defs (Join a b) = Join (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (Unless a b) = Unless (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (Map a b) = Map (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (Quantile a b) = Quantile (resolveExpr' defs a) (resolveExpr' defs b)
resolveExpr' defs (Range a b c d) =
  Range (resolveExpr' defs a) (resolveExpr' defs b) (resolveExpr' defs c) (resolveExpr' defs <$> d)
resolveExpr' defs (QuantileBy a b c) = QuantileBy a (resolveExpr' defs b) (resolveExpr' defs c)
resolveExpr' defs (Expr.Hole idx) =
  case Map.lookup idx defs of
    Just (ExprHoleInst rhs _) -> resolveExpr' defs rhs
    _                         -> Expr.Hole idx
