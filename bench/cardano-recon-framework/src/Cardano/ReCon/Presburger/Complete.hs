{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Cardano.ReCon.Presburger.Complete(IntBinRel(..), Complete(..), unfoldImplies) where

import           Cardano.ReCon.Presburger.IntTerm.Types

import           Data.Word (Word64)

data IntBinRel = IntEq | IntLt | IntLte | IntGt | IntGte deriving (Show, Eq, Ord)

data Complete =
     -- | φ ∨ ψ
     Or Complete Complete
     -- | φ ∧ ψ
   | And Complete Complete
     -- | ¬ φ
   | Not Complete
     -- | φ ⇒ ψ
   | Implies Complete Complete
     -- | T
   | Top
     -- | ⊥
   | Bottom
     -- | ∀x ∈ ℤ. φ
   | IntForall IntIdentifier Complete
     -- | ∃x ∈ ℤ. φ
   | IntExists IntIdentifier Complete
     -- | i = i | i < i | i ≤ i | i > i | i ≥ i
   | IntBinRel IntBinRel IntTerm IntTerm
     -- | ℕ⁺ \| i  (divisibility relation, first argument is a non-zero ℕ)
   | IntDiv Word64 IntTerm deriving (Show, Eq, Ord)

unfoldImplies :: Complete -> Complete -> Complete
unfoldImplies a b = Or (Not a) b
