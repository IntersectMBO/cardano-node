module Cardano.ReCon.Presburger.CompNF(CompNF(..), fromNegNF) where
import qualified Cardano.ReCon.Presburger.Complete as BinRel (IntBinRel (..))
import           Cardano.ReCon.Presburger.IntTerm.Types
import           Cardano.ReCon.Presburger.NegNF (NegNF)
import qualified Cardano.ReCon.Presburger.NegNF as N

import           Data.Word (Word64)

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
   | IntDiv Word64 IntTerm
     -- | ¬ (ℕ⁺ \| i)
   | IntNegDiv Word64 IntTerm
   deriving (Show, Eq, Ord)

fromNegNF :: NegNF -> CompNF
fromNegNF (N.Or a b) = Or (fromNegNF a) (fromNegNF b)
fromNegNF (N.And a b) = And (fromNegNF a) (fromNegNF b)
fromNegNF N.Bottom = Bottom
fromNegNF N.Top = Top
-- s = t <=> s < t + 1 ∧ t < s + 1
fromNegNF (N.IntBinRel BinRel.IntEq s t) = And (IntLt s (IntSum t (IntConst 1))) (IntLt t (IntSum s (IntConst 1)))
fromNegNF (N.IntBinRel BinRel.IntLt s t) = IntLt s t
-- s ≤ t <=> s < t + 1
fromNegNF (N.IntBinRel BinRel.IntLte s t) = IntLt s (IntSum t (IntConst 1))
-- s > t <=> t < s
fromNegNF (N.IntBinRel BinRel.IntGt s t) = IntLt t s
-- s ≥ t <=> t < s + 1
fromNegNF (N.IntBinRel BinRel.IntGte s t) = IntLt t (IntSum s (IntConst 1))
-- ¬ (s = t) <=> s < t ∨ t < s
fromNegNF (N.IntNegBinRel BinRel.IntEq s t) = Or (IntLt s t) (IntLt t s)
-- ¬ (s < t) <=> t < s + 1
fromNegNF (N.IntNegBinRel BinRel.IntLt s t) = IntLt t (IntSum s (IntConst 1))
-- ¬ (s ≤ t) <=> s > t <=> t < s
fromNegNF (N.IntNegBinRel BinRel.IntLte s t) = IntLt t s
-- ¬ (s > t) <=> s ≤ t <=> s < t + 1
fromNegNF (N.IntNegBinRel BinRel.IntGt s t) = IntLt s (IntSum t (IntConst 1))
-- ¬ (s ≥ t) <=> s < t
fromNegNF (N.IntNegBinRel BinRel.IntGte s t) = IntLt s t
fromNegNF (N.IntDiv k t) = IntDiv k t
fromNegNF (N.IntNegDiv k t) = IntNegDiv k t
