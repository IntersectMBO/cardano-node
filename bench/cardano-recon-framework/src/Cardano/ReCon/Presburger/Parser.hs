{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Cardano.ReCon.Presburger.Parser (Parser, formula, intTerm) where

import           Cardano.ReCon.Common.Parser
import           Cardano.ReCon.Common.Types
import           Cardano.ReCon.Integer.Polynomial.Parser (intTerm)
import           Cardano.ReCon.Presburger.Formula (Formula (..))

import           Data.Functor (void)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, space, string)

-- ---------------------------------------------------------------------------
-- Formula parser
-- ---------------------------------------------------------------------------

-- | @d ∣ t@: divisibility constraint.  @d@ must be a non-zero nat.
intDivConstraint :: Parser Formula
intDivConstraint = do
  d <- parseNatValue
  space
  void $ char '∣'
  space
  t <- intTerm
  pure (IntDiv d t)

-- | @t₁ rel t₂@: binary relation constraint.
intRelConstraint :: Parser Formula
intRelConstraint = do
  lhs <- intTerm
  space
  rel <- parseBinRel
  space
  rhs <- intTerm
  pure (IntBinRel rel lhs rhs)

-- | Atom: ⊥, ⊤, parenthesised formula, or an arithmetic constraint.
formulaAtom :: Parser Formula
formulaAtom =
      Bottom <$ string "⊥"
  <|> Top    <$ string "⊤"
  <|> (char '(' *> space *> formula <* space <* char ')')
  <|> try intDivConstraint
  <|> intRelConstraint

-- | Prefix negation applied to a single atom.
formulaNot :: Parser Formula
formulaNot =
      Not <$> (string "¬" *> space *> formulaAtom)
  <|> formulaAtom

-- | Right-associative conjunction @φ ∧ ψ@.
formulaAnd :: Parser Formula
formulaAnd = apply <$> (formulaNot <* space) <*> optional rhs
  where
    rhs = string "∧" *> space *> formulaAnd
    apply phi Nothing    = phi
    apply phi (Just psi) = And phi psi

-- | Right-associative disjunction @φ ∨ ψ@.
formulaOr :: Parser Formula
formulaOr = apply <$> (formulaAnd <* space) <*> optional rhs
  where
    rhs = string "∨" *> space *> formulaOr
    apply phi Nothing    = phi
    apply phi (Just psi) = Or phi psi

-- | Right-associative implication @φ ⇒ ψ@.
formulaImplies :: Parser Formula
formulaImplies = apply <$> (formulaOr <* space) <*> optional rhs
  where
    rhs = string "⇒" *> space *> formulaImplies
    apply phi Nothing    = phi
    apply phi (Just psi) = Implies phi psi

-- | @∀x. φ@: universal integer quantification.
formulaForall :: Parser Formula
formulaForall = do
  void $ string "∀"
  space
  x <- parseIdentifier
  space
  void $ char '.'
  space
  phi <- formula
  pure (IntForall x phi)

-- | @∃x. φ@: existential integer quantification.
formulaExists :: Parser Formula
formulaExists = do
  void $ string "∃"
  space
  x <- parseIdentifier
  space
  void $ char '.'
  space
  phi <- formula
  pure (IntExists x phi)

-- | Parse a Presburger arithmetic formula.
--
-- Precedence table (outermost / lowest binding first):
--
-- @
--   ∀x. φ   ∃x. φ     quantifiers
--   φ ⇒ ψ             implication (right-associative)
--   φ ∨ ψ             disjunction (right-associative)
--   φ ∧ ψ             conjunction (right-associative)
--   ¬ φ               negation    (prefix, binds to single atom)
--   ⊥  ⊤  (φ)  d∣t  t rel t     atoms
-- @
--
-- Note: to combine a quantifier with a connective write
-- @φ ∧ (∀x. ψ)@ — quantifiers inside sub-formulas must be parenthesised.
formula :: Parser Formula
formula =
      formulaForall
  <|> formulaExists
  <|> formulaImplies
