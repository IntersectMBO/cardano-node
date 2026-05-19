module Cardano.ReCon.LTL.Internal.Occurs (
    occursIntTerm
  , occursTextTerm
  , occursFormula
  ) where

import           Cardano.ReCon.LTL.Formula

import           Prelude hiding (Foldable (..))

import           Data.Foldable (foldl')

-- The file concerns checking free variable occurrence in a formula.

-- | Check if the `VariableIdentifier` occurs freely in an `IntTerm`.
occursIntTerm :: VariableIdentifier -> IntTerm -> Bool
occursIntTerm _ (IntConst _)   = False
occursIntTerm x (IntVar _ x')  = x == x'
occursIntTerm x (IntSum a b)   = occursIntTerm x a || occursIntTerm x b

-- | Check if the `VariableIdentifier` occurs freely in a `TextTerm`.
occursTextTerm :: VariableIdentifier -> TextTerm -> Bool
occursTextTerm _ (TextConst _) = False
occursTextTerm x (TextVar x')  = x == x'

-- | Check if the `VariableIdentifier` occurs freely in a `PropConstraint`.
occursPropConstraint :: VariableIdentifier -> PropConstraint -> Bool
occursPropConstraint x (IntPropConstraint  _ t) = occursIntTerm  x t
occursPropConstraint x (TextPropConstraint _ t) = occursTextTerm x t

-- | Check if the `VariableIdentifier` occurs freely in the `Formula`.
occursFormula :: VariableIdentifier -> Formula event a -> Bool
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
occursFormula x (PropIntForall x' _)  | x == x' = False
occursFormula x (PropIntForall _ phi)            = occursFormula x phi
occursFormula x (PropTextForall x' _) | x == x' = False
occursFormula x (PropTextForall _ phi)           = occursFormula x phi
occursFormula x (PropIntForallN x' _ _)  | x == x' = False
occursFormula x (PropIntForallN _ _ phi)            = occursFormula x phi
occursFormula x (PropTextForallN x' _ _) | x == x' = False
occursFormula x (PropTextForallN _ _ phi)           = occursFormula x phi
occursFormula x (PropIntExists x' _)  | x == x' = False
occursFormula x (PropIntExists _ phi)            = occursFormula x phi
occursFormula x (PropTextExists x' _) | x == x' = False
occursFormula x (PropTextExists _ phi)           = occursFormula x phi
occursFormula x (PropIntExistsN x' _ _)  | x == x' = False
occursFormula x (PropIntExistsN _ _ phi)            = occursFormula x phi
occursFormula x (PropTextExistsN x' _ _) | x == x' = False
occursFormula x (PropTextExistsN _ _ phi)           = occursFormula x phi
occursFormula x (Atom _ is) = foldl' (\acc c -> acc || occursPropConstraint x c) False is
occursFormula x (PropIntBinRel _ _ lhs rhs) = occursIntTerm x lhs || occursIntTerm x rhs
occursFormula x (PropTextEq _ t _) = occursTextTerm x t
