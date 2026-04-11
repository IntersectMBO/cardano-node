module Cardano.ReCon.Presburger.NegNF(NegNF(..), fromQuantifierFree) where

import           Cardano.ReCon.Presburger.Complete (IntBinRel)
import           Cardano.ReCon.Presburger.IntTerm.Types
import           Cardano.ReCon.Presburger.QuantifierFree (QuantifierFree, unfoldImplies)
import qualified Cardano.ReCon.Presburger.QuantifierFree as Q

import           Data.Word (Word64)

data NegNF =
     -- | φ ∨ ψ
     Or NegNF NegNF
     -- | φ ∧ ψ
   | And NegNF NegNF
     -- | T
   | Top
     -- | ⊥
   | Bottom
     -- | i = i | i < i | i ≤ i | i > i | i ≥ i
   | IntBinRel IntBinRel IntTerm IntTerm
     -- | ¬ (i = i) | ¬ (i < i) | ¬ (i ≤ i) | ¬ (i > i) | ¬ (i ≥ i)
   | IntNegBinRel IntBinRel IntTerm IntTerm
     -- | ℕ⁺ \| i  (divisibility relation, first argument is a non-zero ℕ)
   | IntDiv Word64 IntTerm
     -- | ¬ (ℕ⁺ \| i)
   | IntNegDiv Word64 IntTerm
   deriving (Show, Eq, Ord)

fromQuantifierFreeNeg :: QuantifierFree -> NegNF
fromQuantifierFreeNeg (Q.Or a b) = And (fromQuantifierFreeNeg a) (fromQuantifierFreeNeg b)
fromQuantifierFreeNeg (Q.And a b) = Or (fromQuantifierFreeNeg a) (fromQuantifierFreeNeg b)
fromQuantifierFreeNeg Q.Top = Bottom
fromQuantifierFreeNeg Q.Bottom = Top
fromQuantifierFreeNeg (Q.IntBinRel r i i') = IntNegBinRel r i i'
fromQuantifierFreeNeg (Q.IntDiv r i) = IntNegDiv r i
fromQuantifierFreeNeg (Q.Not t) = fromQuantifierFree t
fromQuantifierFreeNeg (Q.Implies a b) = fromQuantifierFreeNeg (unfoldImplies a b)

fromQuantifierFree :: QuantifierFree -> NegNF
fromQuantifierFree (Q.Or a b) = Or (fromQuantifierFree a) (fromQuantifierFree b)
fromQuantifierFree (Q.And a b) = And (fromQuantifierFree a) (fromQuantifierFree b)
fromQuantifierFree Q.Top = Top
fromQuantifierFree Q.Bottom = Bottom
fromQuantifierFree (Q.IntBinRel r i i') = IntBinRel r i i'
fromQuantifierFree (Q.IntDiv r i) = IntDiv r i
fromQuantifierFree (Q.Not t) = fromQuantifierFreeNeg t
fromQuantifierFree (Q.Implies a b) = fromQuantifierFree (unfoldImplies a b)
