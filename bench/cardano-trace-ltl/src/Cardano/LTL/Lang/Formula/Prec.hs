module Cardano.LTL.Lang.Formula.Prec where

-- | Finite type of precedences used for formula pretty-printing.
data Prec = Universe | Implies | Or | And | Prefix | Eq | Atom deriving (Show, Eq, Ord)
