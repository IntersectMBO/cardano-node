module Cardano.LTL.Lang.Fragment.Fragment0(Fragment0(..), andList, orList) where
import           Cardano.LTL.Lang.Formula (Relevance)
import           Data.List                (foldl')

-- | t ::= ☐ | ¬ t | t ∧ t | t ∨ t | t ⇒ t | ⊤ | ⊥
--   NOTE: "☐" here stands for "atom".
data Fragment0 event ty = Atom (Relevance event ty)
                        | Not (Fragment0 event ty)
                        | And (Fragment0 event ty) (Fragment0 event ty)
                        | Or (Fragment0 event ty) (Fragment0 event ty)
                        | Implies (Fragment0 event ty) (Fragment0 event ty)
                        | Top
                        | Bottom

-- t₁ ∧ t₂ ∧ ... ∧ tₙ
andList :: [Fragment0 event ty] -> Fragment0 event ty
andList = foldl' And Top

-- t₁ ∨ t₂ ∨ ... ∨ tₙ
orList :: [Fragment0 event ty] -> Fragment0 event ty
orList = foldl' Or Bottom
