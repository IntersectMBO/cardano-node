{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Cardano.ReCon.Presburger.Formula(BinRel(..), Formula(..), unfoldImplies) where

import           Cardano.ReCon.Common.Types (BinRel (..), NatValue)
import           Cardano.ReCon.Integer.Polynomial.Term

-- | Formulae of Presburger arithmetic — the first-order theory of the integers
--   under addition (@+@), integer constants, and the standard comparison
--   relations (@=@, @<@, @≤@, @>@, @≥@), extended with the divisibility
--   predicate @d | t@ ("@d@ divides @t@").
--
--   Quantifiers range over ℤ only.  There are no free variables: every
--   well-formed formula is a closed sentence whose truth value is decidable
--   via Cooper's quantifier-elimination procedure
--   ('Cardano.ReCon.Presburger.Decide.decide').
data Formula =
     -- | φ ∨ ψ
     Or Formula Formula
     -- | φ ∧ ψ
   | And Formula Formula
     -- | ¬ φ
   | Not Formula
     -- | φ ⇒ ψ
   | Implies Formula Formula
     -- | T
   | Top
     -- | ⊥
   | Bottom
     -- | ∀x : ℤ. φ
   | IntForall VariableIdentifier Formula
     -- | ∃x : ℤ. φ
   | IntExists VariableIdentifier Formula
     -- | i = i | i < i | i ≤ i | i > i | i ≥ i
   | IntBinRel BinRel IntTerm IntTerm
     -- | ℕ⁺ \| i  (divisibility relation, first argument is a non-zero ℕ)
   | IntDiv NatValue IntTerm deriving (Show, Eq, Ord)

unfoldImplies :: Formula -> Formula -> Formula
unfoldImplies a b = Or (Not a) b
