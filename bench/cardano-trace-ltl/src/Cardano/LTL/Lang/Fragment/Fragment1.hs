module Cardano.LTL.Lang.Fragment.Fragment1(Fragment1(..), not) where

import           Cardano.LTL.Lang.Formula (Relevance)
import           Prelude                  hiding (not)

-- | t ::= ☐ | ¬☐ | t ∧ t | t ∨ t | ⊤ | ⊥
--   NOTE: "☐" here stands for "atom".
data Fragment1 event ty = Atom (Relevance event ty)
                        | NotAtom (Relevance event ty)
                        | And (Fragment1 event ty) (Fragment1 event ty)
                        | Or (Fragment1 event ty) (Fragment1 event ty)
                        | Top
                        | Bottom

-- | ¬ t
not :: Fragment1 event ty -> Fragment1 event ty
not (Atom set)    = NotAtom set
not (NotAtom set) = Atom set
not (And a b)     = Or (not a) (not b)
not (Or a b)      = And (not a) (not b)
not Top           = Bottom
not Bottom        = Top
