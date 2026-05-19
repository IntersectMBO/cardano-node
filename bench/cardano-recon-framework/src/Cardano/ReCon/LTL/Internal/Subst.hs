module Cardano.ReCon.LTL.Internal.Subst (
    substIntTerm
  , substTextTerm
  , substIntPropConstraint
  , substTextPropConstraint
  , substIntFormula
  , substTextFormula
  ) where

import           Cardano.ReCon.LTL.Formula

import qualified Data.Set as Set

-- The file concerns capture-avoiding substitution of an integer or text value
-- for a `VariableIdentifier` in formulas and their constituents.

-- | t[v / x]  (integer)
substIntTerm :: IntValue -> VariableIdentifier -> IntTerm -> IntTerm
substIntTerm v x (IntVar k x') | x == x' = IntConst (k * v)
substIntTerm v x (IntSum a b)             = IntSum (substIntTerm v x a) (substIntTerm v x b)
substIntTerm _ _ t                        = t

-- | t[v / x]  (text)
substTextTerm :: TextValue -> VariableIdentifier -> TextTerm -> TextTerm
substTextTerm v x (TextVar x') | x == x' = TextConst v
substTextTerm _ _ t = t

-- | c[v / x]  (integer, propagates only into IntPropConstraint)
substIntPropConstraint :: IntValue -> VariableIdentifier -> PropConstraint -> PropConstraint
substIntPropConstraint v x (IntPropConstraint k t)   = IntPropConstraint k (substIntTerm v x t)
substIntPropConstraint _ _ c@(TextPropConstraint {}) = c

-- | c[v / x]  (text, propagates only into TextPropConstraint)
substTextPropConstraint :: TextValue -> VariableIdentifier -> PropConstraint -> PropConstraint
substTextPropConstraint v x (TextPropConstraint k t) = TextPropConstraint k (substTextTerm v x t)
substTextPropConstraint _ _ c@(IntPropConstraint {}) = c

-- | φ[v / x]  (integer)
substIntFormula :: IntValue -> VariableIdentifier -> Formula event a -> Formula event a
substIntFormula v x (Forall k phi) = Forall k (substIntFormula v x phi)
substIntFormula v x (ForallN k phi) = ForallN k (substIntFormula v x phi)
substIntFormula v x (ExistsN k phi) = ExistsN k (substIntFormula v x phi)
substIntFormula v x (Next phi) = Next (substIntFormula v x phi)
substIntFormula v x (NextN k phi) = NextN k (substIntFormula v x phi)
substIntFormula v x (UntilN k phi psi) = UntilN k (substIntFormula v x phi) (substIntFormula v x psi)
substIntFormula v x (Atom c is) = Atom c (Set.map (substIntPropConstraint v x) is)
substIntFormula v x (And phi psi) = And (substIntFormula v x phi) (substIntFormula v x psi)
substIntFormula v x (Or phi psi) = Or (substIntFormula v x phi) (substIntFormula v x psi)
substIntFormula v x (Implies phi psi) = Implies (substIntFormula v x phi) (substIntFormula v x psi)
substIntFormula v x (Not phi) = Not (substIntFormula v x phi)
substIntFormula _ _ Bottom = Bottom
substIntFormula _ _ Top = Top
substIntFormula _ x (PropIntForall x' e)  | x == x' = PropIntForall x' e
substIntFormula v x (PropIntForall x' e)             = PropIntForall x' (substIntFormula v x e)
substIntFormula _ x (PropTextForall x' e) | x == x' = PropTextForall x' e
substIntFormula v x (PropTextForall x' e)            = PropTextForall x' (substIntFormula v x e)
substIntFormula _ x (PropIntForallN x' dom e)  | x == x' = PropIntForallN x' dom e
substIntFormula v x (PropIntForallN x' dom e)             = PropIntForallN x' dom (substIntFormula v x e)
substIntFormula _ x (PropTextForallN x' dom e) | x == x' = PropTextForallN x' dom e
substIntFormula v x (PropTextForallN x' dom e)            = PropTextForallN x' dom (substIntFormula v x e)
substIntFormula _ x (PropIntExists x' e)  | x == x' = PropIntExists x' e
substIntFormula v x (PropIntExists x' e)             = PropIntExists x' (substIntFormula v x e)
substIntFormula _ x (PropTextExists x' e) | x == x' = PropTextExists x' e
substIntFormula v x (PropTextExists x' e)            = PropTextExists x' (substIntFormula v x e)
substIntFormula _ x (PropIntExistsN x' dom e)  | x == x' = PropIntExistsN x' dom e
substIntFormula v x (PropIntExistsN x' dom e)             = PropIntExistsN x' dom (substIntFormula v x e)
substIntFormula _ x (PropTextExistsN x' dom e) | x == x' = PropTextExistsN x' dom e
substIntFormula v x (PropTextExistsN x' dom e)            = PropTextExistsN x' dom (substIntFormula v x e)
substIntFormula v x (PropIntBinRel rel r lhs rhs) = PropIntBinRel rel r (substIntTerm v x lhs) (substIntTerm v x rhs)
substIntFormula _ _ (PropTextEq rel t rhs)        = PropTextEq rel t rhs

-- | φ[v / x]  (text)
substTextFormula :: TextValue -> VariableIdentifier -> Formula event a -> Formula event a
substTextFormula v x (Forall k phi) = Forall k (substTextFormula v x phi)
substTextFormula v x (ForallN k phi) = ForallN k (substTextFormula v x phi)
substTextFormula v x (ExistsN k phi) = ExistsN k (substTextFormula v x phi)
substTextFormula v x (Next phi) = Next (substTextFormula v x phi)
substTextFormula v x (NextN k phi) = NextN k (substTextFormula v x phi)
substTextFormula v x (UntilN k phi psi) = UntilN k (substTextFormula v x phi) (substTextFormula v x psi)
substTextFormula v x (Atom c is) = Atom c (Set.map (substTextPropConstraint v x) is)
substTextFormula v x (And phi psi) = And (substTextFormula v x phi) (substTextFormula v x psi)
substTextFormula v x (Or phi psi) = Or (substTextFormula v x phi) (substTextFormula v x psi)
substTextFormula v x (Implies phi psi) = Implies (substTextFormula v x phi) (substTextFormula v x psi)
substTextFormula v x (Not phi) = Not (substTextFormula v x phi)
substTextFormula _ _ Bottom = Bottom
substTextFormula _ _ Top = Top
substTextFormula _ x (PropIntForall x' e)  | x == x' = PropIntForall x' e
substTextFormula v x (PropIntForall x' e)             = PropIntForall x' (substTextFormula v x e)
substTextFormula _ x (PropTextForall x' e) | x == x' = PropTextForall x' e
substTextFormula v x (PropTextForall x' e)            = PropTextForall x' (substTextFormula v x e)
substTextFormula _ x (PropIntForallN x' dom e)  | x == x' = PropIntForallN x' dom e
substTextFormula v x (PropIntForallN x' dom e)             = PropIntForallN x' dom (substTextFormula v x e)
substTextFormula _ x (PropTextForallN x' dom e) | x == x' = PropTextForallN x' dom e
substTextFormula v x (PropTextForallN x' dom e)            = PropTextForallN x' dom (substTextFormula v x e)
substTextFormula _ x (PropIntExists x' e)  | x == x' = PropIntExists x' e
substTextFormula v x (PropIntExists x' e)             = PropIntExists x' (substTextFormula v x e)
substTextFormula _ x (PropTextExists x' e) | x == x' = PropTextExists x' e
substTextFormula v x (PropTextExists x' e)            = PropTextExists x' (substTextFormula v x e)
substTextFormula _ x (PropIntExistsN x' dom e)  | x == x' = PropIntExistsN x' dom e
substTextFormula v x (PropIntExistsN x' dom e)             = PropIntExistsN x' dom (substTextFormula v x e)
substTextFormula _ x (PropTextExistsN x' dom e) | x == x' = PropTextExistsN x' dom e
substTextFormula v x (PropTextExistsN x' dom e)            = PropTextExistsN x' dom (substTextFormula v x e)
substTextFormula _ _ (PropIntBinRel rel r t rhs) = PropIntBinRel rel r t rhs
substTextFormula v x (PropTextEq rel t rhs)      = PropTextEq rel (substTextTerm v x t) rhs
