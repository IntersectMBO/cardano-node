module Cardano.ReCon.Presburger.Internal.IR.ForallFree (ForallFree(..), fromFormula) where

import           Cardano.ReCon.Common.Types (NatValue)
import           Cardano.ReCon.Integer.Polynomial.Term
import           Cardano.ReCon.Presburger.Formula (BinRel)
import qualified Cardano.ReCon.Presburger.Formula as C

-- | Presburger formulae with all universal quantifiers eliminated by duality:
--   @∀x. φ  ≡  ¬(∃x. ¬φ)@.
--   Only existential quantifiers remain; the structure is otherwise identical
--   to 'Cardano.ReCon.Presburger.Formula.Formula'.
--   This is the first IR in the Cooper QE pipeline.
data ForallFree =
     -- | φ ∨ ψ
     Or ForallFree ForallFree
     -- | φ ∧ ψ
   | And ForallFree ForallFree
     -- | ¬ φ
   | Not ForallFree
     -- | φ ⇒ ψ
   | Implies ForallFree ForallFree
     -- | T
   | Top
     -- | ⊥
   | Bottom
     -- | ∃x ∈ ℤ. φ
   | IntExists VariableIdentifier ForallFree
     -- | i = i | i < i | i ≤ i | i > i | i ≥ i
   | IntBinRel BinRel IntTerm IntTerm
     -- | ℕ⁺ \| i  (divisibility relation, first argument is a non-zero ℕ)
   | IntDiv NatValue IntTerm
   deriving (Show, Eq, Ord)

-- | Eliminate all universal quantifiers via  ∀x. φ  ≡  ¬(∃x. ¬φ).
fromFormula :: C.Formula -> ForallFree
fromFormula (C.Or a b)          = Or (fromFormula a) (fromFormula b)
fromFormula (C.And a b)         = And (fromFormula a) (fromFormula b)
fromFormula (C.Not a)           = Not (fromFormula a)
fromFormula (C.Implies a b)     = Implies (fromFormula a) (fromFormula b)
fromFormula C.Top               = Top
fromFormula C.Bottom            = Bottom
fromFormula (C.IntExists x φ)   = IntExists x (fromFormula φ)
fromFormula (C.IntForall x φ)   = Not (IntExists x (Not (fromFormula φ)))
fromFormula (C.IntBinRel r i j) = IntBinRel r i j
fromFormula (C.IntDiv k t)      = IntDiv k t
