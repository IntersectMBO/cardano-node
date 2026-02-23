module Cardano.LTL.Subst (
    substPropTerm
  , substPropConstraint
  , substFormula
  ) where

import           Cardano.LTL.Lang.Formula
import qualified Data.Set                 as Set

-- The file concerns capture-avoiding substitution of `PropValue` for `PropVarIdentifier` in formulas and their constituents.

-- | t[v / x]
substPropTerm :: PropValue -> PropVarIdentifier -> PropTerm -> PropTerm
substPropTerm v x (Var x') | x == x' = Const v
substPropTerm _ _ t = t

-- | c[v / x]
substPropConstraint :: PropValue -> PropVarIdentifier -> PropConstraint -> PropConstraint
substPropConstraint v x (PropConstraint k t) = PropConstraint k (substPropTerm v x t)

-- | φ[v / x]
substFormula :: PropValue -> PropVarIdentifier -> Formula event a -> Formula event a
substFormula v x (Forall k phi) = Forall k (substFormula v x phi)
substFormula v x (ForallN k phi) = ForallN k (substFormula v x phi)
substFormula v x (ExistsN k phi) = ExistsN k (substFormula v x phi)
substFormula v x (Next phi) = Next (substFormula v x phi)
substFormula v x (NextN k phi) = NextN k (substFormula v x phi)
substFormula v x (UntilN k phi psi) = UntilN k (substFormula v x phi) (substFormula v x psi)
substFormula v x (Atom c is) = Atom c (Set.map (substPropConstraint v x) is)
substFormula v x (And phi psi) = And (substFormula v x phi) (substFormula v x psi)
substFormula v x (Or phi psi) = Or (substFormula v x phi) (substFormula v x psi)
substFormula v x (Implies phi psi) = Implies (substFormula v x phi) (substFormula v x psi)
substFormula v x (Not phi) = Not (substFormula v x phi)
substFormula _ _ Bottom = Bottom
substFormula _ _ Top = Top
substFormula _ x (PropForall x' e) | x == x' = PropForall x' e
substFormula v x (PropForall x' e) = PropForall x' (substFormula v x e)
substFormula _ x (PropForallN x' dom e) | x == x' = PropForallN x' dom e
substFormula v x (PropForallN x' dom e) = PropForallN x' dom (substFormula v x e)
substFormula v x (PropEq rel t rhs) = PropEq rel (substPropTerm v x t) rhs
