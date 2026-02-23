{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Cardano.LTL.Lang.HomogeneousFormula (
    HomogeneousFormula(..)
  , toFormula
  , values
  , substHomogeneousFormula
  , eval
  , quote
  , equiv
  , retract
  , normaliseHomogeneous) where

import           Cardano.LTL.Lang.Formula (Formula, PropTerm (..), PropValue,
                                           PropVarIdentifier, Relevance)
import qualified Cardano.LTL.Lang.Formula as F
import           Data.Function            (on)
import           Data.Functor             ((<&>))
import           Data.List                (foldl')
import           Data.Set                 (Set)
import qualified Data.Set                 as Set

data ExtendedPropValue = Val PropValue | Placeholder deriving (Show, Ord, Eq)

-- | A `Formula` with no temporal operators.
--   Equivalence of two `HomogeneousFormula`s is decidable.
data HomogeneousFormula event ty =
   ------------ Connective -------------
     Or (HomogeneousFormula event ty) (HomogeneousFormula event ty)
   | And (HomogeneousFormula event ty) (HomogeneousFormula event ty)
   | Not (HomogeneousFormula event ty)
   | Implies (HomogeneousFormula event ty) (HomogeneousFormula event ty)
   | Top
   | Bottom
   -------------------------------------


   ----------- Event property ----------
   | PropForall PropVarIdentifier (HomogeneousFormula event ty)
   | PropForallN PropVarIdentifier (Set PropValue) (HomogeneousFormula event ty)
   | PropEq (Relevance event ty) PropTerm PropValue deriving (Show, Eq, Ord)
   -------------------------------------

toFormula :: HomogeneousFormula event ty -> Formula event ty
toFormula (And a b)               = F.And (toFormula a) (toFormula b)
toFormula (Or a b)                = F.Or (toFormula a) (toFormula b)
toFormula (Implies a b)           = F.Implies (toFormula a) (toFormula b)
toFormula (Not a)                 = F.Not (toFormula a)
toFormula Bottom                  = F.Bottom
toFormula Top                     = F.Top
toFormula (PropEq e a b)          = F.PropEq e a b
toFormula (PropForall x phi)      = F.PropForall x (toFormula phi)
toFormula (PropForallN x dom phi) = F.PropForallN x dom (toFormula phi)

valuesAccum :: Set PropValue -> PropVarIdentifier -> HomogeneousFormula event ty -> Set PropValue
valuesAccum acc x (Or phi psi) = valuesAccum (valuesAccum acc x phi) x psi
valuesAccum acc x (And phi psi) = valuesAccum (valuesAccum acc x phi) x psi
valuesAccum acc x (Not phi) = valuesAccum acc x phi
valuesAccum acc x (Implies phi psi) = valuesAccum (valuesAccum acc x phi) x psi
valuesAccum acc _ Top = acc
valuesAccum acc _ Bottom = acc
valuesAccum acc _ (PropEq _ (F.Const _) _) = acc
valuesAccum acc x (PropEq _ (F.Var x') v) | x == x' = Set.insert v acc
valuesAccum acc _ (PropEq _ (F.Var _) _) = acc
valuesAccum acc x (PropForall x' phi) | x /= x' = valuesAccum acc x phi
valuesAccum acc _ (PropForall _ _) = acc
valuesAccum acc x (PropForallN x' _ phi) | x /= x' = valuesAccum acc x phi
valuesAccum acc _ (PropForallN {}) = acc

-- | Set of values the given prop var can take in the formula.
values :: PropVarIdentifier -> HomogeneousFormula event ty -> Set PropValue
values = valuesAccum Set.empty

-- | φ[v / x]
substHomogeneousFormula :: ExtendedPropValue -> PropVarIdentifier -> HomogeneousFormula event ty -> HomogeneousFormula event ty
substHomogeneousFormula v x (And phi psi) = And (substHomogeneousFormula v x phi) (substHomogeneousFormula v x psi)
substHomogeneousFormula v x (Or phi psi) = Or (substHomogeneousFormula v x phi) (substHomogeneousFormula v x psi)
substHomogeneousFormula v x (Implies phi psi) = Implies (substHomogeneousFormula v x phi) (substHomogeneousFormula v x psi)
substHomogeneousFormula v x (Not phi) = Not (substHomogeneousFormula v x phi)
substHomogeneousFormula _ _ Bottom = Bottom
substHomogeneousFormula _ _ Top = Top
substHomogeneousFormula _ _ (PropEq rel (F.Const v') rhs) = PropEq rel (F.Const v') rhs
-- (x = v)[☐ / x] = ⊥
-- i.e. the placeholder value is distinct from all possible `PropValue`s
substHomogeneousFormula Placeholder x (PropEq _ (F.Var x') _) | x == x' = Bottom
substHomogeneousFormula (Val v) x (PropEq rel (F.Var x') rhs) | x == x' = PropEq rel (F.Const v) rhs
substHomogeneousFormula _ _ (PropEq rel (F.Var x') rhs) = PropEq rel (F.Var x') rhs
substHomogeneousFormula v x (PropForall x' phi) | x /= x' = PropForall x' (substHomogeneousFormula v x phi)
substHomogeneousFormula _ _ (PropForall x' phi) = PropForall x' phi
substHomogeneousFormula v x (PropForallN x' dom phi) | x /= x' = PropForallN x' dom (substHomogeneousFormula v x phi)
substHomogeneousFormula _ _ (PropForallN x' dom phi) = PropForallN x' dom phi

-- | Evaluate the `HomogeneousFormula` onto `Bool`.
--   This is the "interesting" part of the iso: `HomogeneousFormula` ≅ `Bool`
eval :: HomogeneousFormula event ty -> Bool
eval (Or phi psi) = eval phi || eval psi
eval (And phi psi) = eval phi && eval psi
eval (Not phi) = not (eval phi)
eval (Implies phi psi) = not (eval phi) || eval psi
eval Bottom = False
eval Top = True
eval (PropEq _ (F.Const lhs) rhs) = lhs == rhs
eval (PropEq _ (F.Var x) _) = error $ "interp: free variable " <> show x
-- ⟦∀x. φ⟧ <=> φ[☐/x] ∧ φ[v₁ / x] ∧ ... ∧ φ[vₖ / x] where v₁...vₖ is the set of values in φ which x can take.
eval (PropForall x phi) = eval (substHomogeneousFormula Placeholder x phi) &&
  foldl' (&&) True (
    Set.toList (values x phi) <&> \v ->
      eval (substHomogeneousFormula (Val v) x phi)
  )
-- ⟦∀(x ∈ v₁...vₖ). φ⟧ <=> φ[v₁ / x] ∧ ... ∧ φ[vₖ / x]
eval (PropForallN x dom phi) =
  foldl' (&&) True (
    Set.toList dom <&> \v ->
      eval (substHomogeneousFormula (Val v) x phi)
  )

-- | This is the "easy" part of the iso: `HomogeneousFormula` ≅ `Bool`
quote :: Bool -> HomogeneousFormula event ty
quote True  = Top
quote False = Bottom

-- | Check equivalence of two `HomogeneousFormula`s.
equiv :: HomogeneousFormula event ty -> HomogeneousFormula event ty -> Bool
equiv = on (==) eval

retract :: Formula event ty -> Maybe (HomogeneousFormula event ty)
retract = go Set.empty where
  go :: Set PropVarIdentifier -> Formula event ty -> Maybe (HomogeneousFormula event ty)
  go _     (F.ForallN {})                = Nothing
  go _     (F.ExistsN {})                = Nothing
  go _     (F.Forall {})                 = Nothing
  go _     (F.NextN {})                  = Nothing
  go _     (F.UntilN {})                 = Nothing
  go _     (F.Atom {})                   = Nothing
  go _     (F.Next _)                    = Nothing
  go bound (F.And phi psi)               = And <$> go bound phi <*> go bound psi
  go bound (F.Or phi psi)                = Or <$> go bound phi <*> go bound psi
  go bound (F.Implies phi psi)           = Implies <$> go bound phi <*> go bound psi
  go bound (F.Not phi)                   = Not <$> go bound phi
  go _     F.Bottom                      = Just Bottom
  go _     F.Top                         = Just Top
  go _     (F.PropEq rel (Const v') v)   = Just (PropEq rel (Const v') v)
  go bound (F.PropEq rel (Var x) v)
                | Set.member x bound     = Just (PropEq rel (Var x) v)
  go _     (F.PropEq _ (Var _) _)        = Nothing
  go bound (F.PropForall x phi)          = PropForall x <$> go (Set.insert x bound) phi
  go bound (F.PropForallN x dom phi)     = PropForallN x dom <$> go (Set.insert x bound) phi

normaliseHomogeneous :: Formula event ty -> Maybe (Formula event ty)
normaliseHomogeneous phi =
  toFormula . quote . eval <$> retract phi
