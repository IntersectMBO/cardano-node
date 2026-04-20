module Cardano.ReCon.Presburger.Internal.IR.NegNF(NegNF(..), fromQuantifierFree) where

import           Cardano.ReCon.Common.Types (NatValue)
import           Cardano.ReCon.Integer.Polynomial.Term
import           Cardano.ReCon.Presburger.Formula (BinRel)
import           Cardano.ReCon.Presburger.Internal.IR.QuantifierFree (QuantifierFree, unfoldImplies)
import qualified Cardano.ReCon.Presburger.Internal.IR.QuantifierFree as Q

-- | Formulae in negation normal form (NNF): all negations have been pushed
--   inward past connectives via De Morgan's laws, so negation appears only at
--   the atomic level as 'IntNegBinRel' (negated comparison) or 'IntNegDiv'
--   (negated divisibility).  No @Not@ connective remains.
--   Obtained from 'Cardano.ReCon.Presburger.Internal.IR.QuantifierFree.QuantifierFree' via 'fromQuantifierFree'.
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
   | IntBinRel BinRel IntTerm IntTerm
     -- | ¬ (i = i) | ¬ (i < i) | ¬ (i ≤ i) | ¬ (i > i) | ¬ (i ≥ i)
   | IntNegBinRel BinRel IntTerm IntTerm
     -- | ℕ⁺ \| i  (divisibility relation, first argument is a non-zero ℕ)
   | IntDiv NatValue IntTerm
     -- | ¬ (ℕ⁺ \| i)
   | IntNegDiv NatValue IntTerm
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
