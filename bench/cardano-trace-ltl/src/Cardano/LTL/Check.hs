{- HLINT ignore "Use newtype instead of data" -}
module Cardano.LTL.Check(
    checkParamTerm
  , checkParamConstraint
  , checkFormula
  , Error(..)
  ) where

import           Cardano.LTL.Lang.Formula
import           Data.List                (foldl')
import           Data.Set                 (Set, insert, member)
import qualified Data.Set                 as Set
import           Data.Text                (Text)

-- The file concerns checking syntactic correctness of formulas.

data Error = UnboundPropVarIdentifier PropVarIdentifier deriving (Show, Eq)

-- | Verify the given parameter variable is bound in the current context.
checkParamVar :: Set PropVarIdentifier -> Text -> [Error]
checkParamVar bound x | member x bound  = []
checkParamVar _ x  = [UnboundPropVarIdentifier x]

-- | Validate a term by checking all of its free variables are bound.
checkParamTerm :: Set PropVarIdentifier -> PropTerm -> [Error]
checkParamTerm bound (Var x) = checkParamVar bound x
checkParamTerm _ (Const _)   = []

-- | Validate a constraint by ensuring its term is well-scoped.
checkParamConstraint :: Set PropVarIdentifier -> PropConstraint -> [Error]
checkParamConstraint bound (PropConstraint _ t) = checkParamTerm bound t

-- | Check whether the formula is syntactically valid:
-- |  — all parameter variables shall be universally bound
checkFormula :: Set PropVarIdentifier -> Formula event ty -> [Error]
checkFormula bound (Forall _ phi) = checkFormula bound phi
checkFormula bound (ForallN _ phi) = checkFormula bound phi
checkFormula bound (ExistsN _ phi) = checkFormula bound phi
checkFormula bound (And phi psi) = checkFormula bound phi ++ checkFormula bound psi
checkFormula bound (Or phi psi) = checkFormula bound phi ++ checkFormula bound psi
checkFormula bound (Not phi) = checkFormula bound phi
checkFormula _ Bottom = []
checkFormula _ Top = []
checkFormula bound (Next phi) = checkFormula bound phi
checkFormula bound (NextN _ phi) = checkFormula bound phi
checkFormula bound (Implies phi psi) = checkFormula bound phi ++ checkFormula bound psi
checkFormula bound (UntilN _ phi psi) = checkFormula bound phi ++ checkFormula bound psi
checkFormula bound (PropForall x phi) = checkFormula (insert x bound) phi
checkFormula bound (PropForallN x _ phi) = checkFormula (insert x bound) phi
checkFormula bound (PropEq _ t _) = checkParamTerm bound t
checkFormula bound (Atom _ cs) = foldl' (++) [] (fmap (checkParamConstraint bound) (Set.toList cs))
