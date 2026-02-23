module Cardano.LTL.Lang.Fragment.Fragment2(Fragment2(..), and, or) where

import           Cardano.LTL.Lang.Formula (Relevance)
import           Data.Set                 (union)
import           Prelude                  hiding (and, or)

-- | t ::= ☐ | ¬☐ | ⊤ | ⊥
--   NOTE: "☐" here stands for "atom".
data Fragment2 event ty = Atom (Relevance event ty)
                        | NotAtom (Relevance event ty)
                        | Top
                        | Bottom

-- | t₀ ∧ t₁
and :: (Ord event, Ord ty) => Fragment2 event ty -> Fragment2 event ty -> Fragment2 event ty
and (Atom rel)    (Atom rel')    = Atom (rel `union` rel')
and (Atom _)      (NotAtom _)    = Bottom
and (Atom _)      Bottom         = Bottom
and (Atom rel)    Top            = Atom rel
and (NotAtom _)   (Atom _)       = Bottom
and (NotAtom rel) (NotAtom rel') = NotAtom (rel `union` rel')
and (NotAtom rel) Top            = NotAtom rel
and (NotAtom _)   Bottom         = Bottom
and Top           t              = t
and Bottom        _              = Bottom

-- | t₀ ∨ t₁
or :: (Ord event, Ord ty) => Fragment2 event ty -> Fragment2 event ty -> Fragment2 event ty
or (Atom rel)    (Atom rel')    = Atom (rel `union` rel')
or (Atom _)      (NotAtom _)    = Top
or (Atom rel)    Bottom         = Atom rel
or (Atom _)      Top            = Top
or (NotAtom _)   (Atom _)       = Top
or (NotAtom rel) (NotAtom rel') = NotAtom (rel `union` rel')
or (NotAtom _)   Top            = Top
or (NotAtom rel) Bottom         = NotAtom rel
or Bottom        t              = t
or Top           _              = Top
