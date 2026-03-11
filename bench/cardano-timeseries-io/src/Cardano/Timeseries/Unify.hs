{- HLINT ignore "Use newtype instead of data" -}
module Cardano.Timeseries.Unify(UnificationProblem(..), St(..), UnifyM, unify, solve) where
import           Cardano.Timeseries.Query.Expr (HoleIdentifier)
import           Cardano.Timeseries.Resolve
import           Cardano.Timeseries.Surface.Expr (Loc)
import           Cardano.Timeseries.Typing (Defs, Ty (..), TyPrec (Loose), instantiateTy, prettyTy)

import           Control.Monad.Except (ExceptT, throwError)
import           Control.Monad.State.Strict (State, get, state)
import           Data.Text (Text, pack)
import           Text.Megaparsec (sourcePosPretty)

-- | A = B type
--   An equation between two query types containing holes.
--   Unification is an algorithm of finding unique solutions to such equations.
data UnificationProblem = UnificationProblem {
  -- | Source location that induces the unification problem.
  loc :: Loc,
  lhs :: Ty,
  rhs :: Ty
}

data St = St {
  defs :: Defs
}

type UnifyError = Text

type UnifyM a = ExceptT UnifyError (State St) a

updateDefs :: Defs -> St -> St
updateDefs defs (St _) = St defs

unifyHole :: HoleIdentifier -> Ty -> UnifyM ()
unifyHole x rhs = do
  occurs x rhs >>= \case
    True -> error $ "[INTERNAL ERROR] Occurs check failed for " <> show x <> " and " <> show rhs
    False -> state $ \st ->
     ((), updateDefs (instantiateTy x rhs (defs st)) st)

-- | Assume the types are head-neutral (i.e. resolved holes have been substituted in if the hole is the outer expression)
unifyNu :: Loc -> Ty -> Ty -> UnifyM [UnificationProblem]
unifyNu loc (Fun a b) (Fun a' b') = pure [UnificationProblem loc a a', UnificationProblem loc b b']
unifyNu loc (InstantVector a) (InstantVector a') = pure [UnificationProblem loc a a']
unifyNu loc (RangeVector a) (RangeVector a') = pure [UnificationProblem loc a a']
unifyNu _ Scalar Scalar = pure []
unifyNu _ Bool Bool = pure []
unifyNu _ Text Text = pure []
unifyNu _ Timestamp Timestamp = pure []
unifyNu _ Duration Duration = pure []
unifyNu loc (Pair a b) (Pair a' b') = pure [UnificationProblem loc a a', UnificationProblem loc b b']
unifyNu _ (Hole x) (Hole y) | x == y = pure []
unifyNu _ (Hole x) ty = [] <$ unifyHole x ty
unifyNu _ ty (Hole y) = [] <$ unifyHole y ty
unifyNu loc lhs rhs =
  throwError $ "Can't solve unification constraint: "
    <> prettyTy Loose lhs
    <> " = "
    <> prettyTy Loose rhs
    <> " @ "
    <> pack (sourcePosPretty loc)

-- | Check if the given hole identifier occurs in the given type.
occurs :: HoleIdentifier -> Ty -> UnifyM Bool
occurs x ty = do
  ds <- defs <$> get
  occursNu x (resolveTy ds ty)

-- | `Ty` is assumed to be normal w.r.t. hole substitution.
occursNu :: HoleIdentifier -> Ty -> UnifyM Bool
occursNu x (InstantVector ty) = occursNu x ty
occursNu x (RangeVector ty)   = occursNu x ty
occursNu x (Fun ty ty')       = (||) <$> occursNu x ty <*> occursNu x ty'
occursNu x (Pair ty ty')      = (||) <$> occursNu x ty <*> occursNu x ty'
occursNu _ Scalar             = pure False
occursNu _ Bool               = pure False
occursNu _ Timestamp          = pure False
occursNu _ Duration           = pure False
occursNu _ Text               = pure False
occursNu x (Hole x')          = pure (x == x')

unify :: Loc -> Ty -> Ty -> UnifyM [UnificationProblem]
unify loc lhs rhs = do
  ds <- defs <$> get
  unifyNu loc (resolveTy ds lhs) (resolveTy ds rhs)

-- | Solve the list of unification problems, instantiating holes in the process.
--   If a problem doesn't have a (unique) solution, throw an error.
solve :: [UnificationProblem] -> UnifyM ()
solve [] = pure ()
solve (UnificationProblem loc lhs rhs : rest) = do
  new <- unify loc lhs rhs
  solve (new ++ rest)
