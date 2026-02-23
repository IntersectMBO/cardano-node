module Cardano.LTL.Occurs (
    occursPropTerm
  , occursPropConstraint
  , occursFormula
  ) where

import           Cardano.LTL.Lang.Formula
import           Data.List                (foldl')

-- The file concerns checking free variable occurrence in a formula.

-- | Check if the `PropVarIdentifier` occurs freely in the `PropTerm`.
occursPropTerm :: PropVarIdentifier -> PropTerm -> Bool
occursPropTerm _ (Const _) = False
occursPropTerm x (Var x')  = x == x'

-- | Check if the `PropVarIdentifier` occurs freely in the `PropConstraint`.
occursPropConstraint :: PropVarIdentifier -> PropConstraint -> Bool
occursPropConstraint x (PropConstraint _ t) = occursPropTerm x t

-- | Check if the `PropVarIdentifier` occurs freely in the `Formula`.
occursFormula :: PropVarIdentifier -> Formula event a -> Bool
occursFormula x (Forall _ phi) = occursFormula x phi
occursFormula x (ForallN _ phi) = occursFormula x phi
occursFormula x (ExistsN _ phi) = occursFormula x phi
occursFormula x (Next phi) = occursFormula x phi
occursFormula x (NextN _ phi) = occursFormula x phi
occursFormula x (UntilN _ phi psi) = occursFormula x phi || occursFormula x psi
occursFormula x (And phi psi) = occursFormula x phi || occursFormula x psi
occursFormula x (Or phi psi) = occursFormula x phi || occursFormula x psi
occursFormula x (Implies phi psi) = occursFormula x phi || occursFormula x psi
occursFormula x (Not phi) = occursFormula x phi
occursFormula _ Bottom = False
occursFormula _ Top = False
occursFormula x (PropForall x' _) | x == x' = False
occursFormula x (PropForall _ phi) = occursFormula x phi
occursFormula x (PropForallN x' _ _) | x == x' = False
occursFormula x (PropForallN _ _ phi) = occursFormula x phi
occursFormula x (Atom _ is) = foldl' (\acc phi -> acc || occursPropConstraint x phi) False is
occursFormula x (PropEq _ t _) = occursPropTerm x t
