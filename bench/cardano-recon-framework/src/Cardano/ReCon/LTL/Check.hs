{- HLINT ignore "Use newtype instead of data" -}
module Cardano.ReCon.LTL.Check(
    checkIntTerm
  , checkTextTerm
  , checkParamConstraint
  , checkFormula
  , Error(..)
  , prettyError
  ) where

import           Cardano.ReCon.LTL.Formula

import           Prelude hiding (Foldable (..))

import           Data.Foldable (foldl')
import           Data.Set (Set, insert, member)
import qualified Data.Set as Set
import           Data.Text (Text)

-- The file concerns checking syntactic correctness of formulas.

data Error = UnboundVariableIdentifier VariableIdentifier deriving (Show, Eq)

prettyError :: Error -> Text
prettyError (UnboundVariableIdentifier x) = "Unbound variable: " <> x

-- | Verify the given parameter variable is bound in the current context.
checkParamVar :: Set VariableIdentifier -> Text -> [Error]
checkParamVar bound x | member x bound  = []
checkParamVar _ x  = [UnboundVariableIdentifier x]

-- | Validate an integer term by checking all of its free variables are bound.
checkIntTerm :: Set VariableIdentifier -> IntTerm -> [Error]
checkIntTerm bound (IntVar _ x) = checkParamVar bound x
checkIntTerm _ (IntConst _)     = []
checkIntTerm bound (IntSum a b) = checkIntTerm bound a ++ checkIntTerm bound b

-- | Validate a text term by checking all of its free variables are bound.
checkTextTerm :: Set VariableIdentifier -> TextTerm -> [Error]
checkTextTerm bound (TextVar x) = checkParamVar bound x
checkTextTerm _ (TextConst _)   = []

-- | Validate a constraint by ensuring its term is well-scoped.
checkParamConstraint :: Set VariableIdentifier -> PropConstraint -> [Error]
checkParamConstraint bound (IntPropConstraint  _ t) = checkIntTerm  bound t
checkParamConstraint bound (TextPropConstraint _ t) = checkTextTerm bound t

-- | Check whether the formula is syntactically valid.
checkFormula :: Set VariableIdentifier -> Formula event ty -> [Error]
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
checkFormula bound (PropIntForall  x phi) = checkFormula (insert x bound) phi
checkFormula bound (PropTextForall x phi) = checkFormula (insert x bound) phi
checkFormula bound (PropIntForallN  x _ phi) = checkFormula (insert x bound) phi
checkFormula bound (PropTextForallN x _ phi) = checkFormula (insert x bound) phi
checkFormula bound (PropIntExists  x phi) = checkFormula (insert x bound) phi
checkFormula bound (PropTextExists x phi) = checkFormula (insert x bound) phi
checkFormula bound (PropIntExistsN  x _ phi) = checkFormula (insert x bound) phi
checkFormula bound (PropTextExistsN x _ phi) = checkFormula (insert x bound) phi
checkFormula bound (PropIntBinRel _ _ lhs rhs) = checkIntTerm bound lhs ++ checkIntTerm bound rhs
checkFormula bound (PropTextEq _ t _) = checkTextTerm bound t
checkFormula bound (Atom _ cs) = foldl' (++) [] (fmap (checkParamConstraint bound) (Set.toList cs))
