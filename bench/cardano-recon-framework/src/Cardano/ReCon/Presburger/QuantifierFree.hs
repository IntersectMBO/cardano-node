{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Cardano.ReCon.Presburger.QuantifierFree(QuantifierFree(..), unfoldImplies) where

import           Cardano.ReCon.Presburger.Complete (IntBinRel)
import           Cardano.ReCon.Presburger.IntTerm.Types

import           Data.Word (Word64)

data QuantifierFree =
     -- | φ ∨ ψ
     Or QuantifierFree QuantifierFree
     -- | φ ∧ ψ
   | And QuantifierFree QuantifierFree
     -- | ¬ φ
   | Not QuantifierFree
     -- | φ ⇒ ψ
   | Implies QuantifierFree QuantifierFree
     -- | T
   | Top
     -- | ⊥
   | Bottom
     -- | i = i | i < i | i ≤ i | i > i | i ≥ i
   | IntBinRel IntBinRel IntTerm IntTerm
     -- | ℕ⁺ \| i  (divisibility relation, first argument is a non-zero ℕ)
   | IntDiv Word64 IntTerm deriving (Show, Eq, Ord)

unfoldImplies :: QuantifierFree -> QuantifierFree -> QuantifierFree
unfoldImplies a b = Or (Not a) b
