module Cardano.ReCon.Presburger.Internal.IR.CompNF(CompNF(..), fromNegNF) where
import           Cardano.ReCon.Common.Types (NatValue)
import           Cardano.ReCon.Integer.Polynomial.Term
import qualified Cardano.ReCon.Presburger.Formula as BinRel (BinRel (..))
import           Cardano.ReCon.Presburger.Internal.IR.NegNF (NegNF)
import qualified Cardano.ReCon.Presburger.Internal.IR.NegNF as N


-- | Formulae in comparison normal form: all comparison relations have been
--   reduced to strict less-than (@<@) only.  The reductions are:
--
--   @s = t  ≡  s < t+1 ∧ t < s+1@,
--   @s ≤ t  ≡  s < t+1@,
--   @s > t  ≡  t < s@,
--   @s ≥ t  ≡  t < s+1@,
--
--   and their negations are unfolded analogously.
--   No @Implies@ or @Not@ connectives remain.
--   Obtained from 'Cardano.ReCon.Presburger.Internal.IR.NegNF.NegNF' via 'fromNegNF'.
data CompNF =
     -- | φ ∨ ψ
     Or CompNF CompNF
     -- | φ ∧ ψ
   | And CompNF CompNF
     -- | T
   | Top
     -- | ⊥
   | Bottom
     -- | i < i
   | IntLt IntTerm IntTerm
     -- | ℕ⁺ \| i  (divisibility relation, first argument is a non-zero ℕ)
   | IntDiv NatValue IntTerm
     -- | ¬ (ℕ⁺ \| i)
   | IntNegDiv NatValue IntTerm
   deriving (Show, Eq, Ord)

fromNegNF :: NegNF -> CompNF
fromNegNF (N.Or a b) = Or (fromNegNF a) (fromNegNF b)
fromNegNF (N.And a b) = And (fromNegNF a) (fromNegNF b)
fromNegNF N.Bottom = Bottom
fromNegNF N.Top = Top
-- s = t <=> s < t + 1 ∧ t < s + 1
fromNegNF (N.IntBinRel BinRel.Eq s t) = And (IntLt s (IntSum t (IntConst 1))) (IntLt t (IntSum s (IntConst 1)))
fromNegNF (N.IntBinRel BinRel.Lt s t) = IntLt s t
-- s ≤ t <=> s < t + 1
fromNegNF (N.IntBinRel BinRel.Lte s t) = IntLt s (IntSum t (IntConst 1))
-- s > t <=> t < s
fromNegNF (N.IntBinRel BinRel.Gt s t) = IntLt t s
-- s ≥ t <=> t < s + 1
fromNegNF (N.IntBinRel BinRel.Gte s t) = IntLt t (IntSum s (IntConst 1))
-- ¬ (s = t) <=> s < t ∨ t < s
fromNegNF (N.IntNegBinRel BinRel.Eq s t) = Or (IntLt s t) (IntLt t s)
-- ¬ (s < t) <=> t < s + 1
fromNegNF (N.IntNegBinRel BinRel.Lt s t) = IntLt t (IntSum s (IntConst 1))
-- ¬ (s ≤ t) <=> s > t <=> t < s
fromNegNF (N.IntNegBinRel BinRel.Lte s t) = IntLt t s
-- ¬ (s > t) <=> s ≤ t <=> s < t + 1
fromNegNF (N.IntNegBinRel BinRel.Gt s t) = IntLt s (IntSum t (IntConst 1))
-- ¬ (s ≥ t) <=> s < t
fromNegNF (N.IntNegBinRel BinRel.Gte s t) = IntLt s t
fromNegNF (N.IntDiv k t) = IntDiv k t
fromNegNF (N.IntNegDiv k t) = IntNegDiv k t
