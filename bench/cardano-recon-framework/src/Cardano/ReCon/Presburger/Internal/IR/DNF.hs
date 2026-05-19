module Cardano.ReCon.Presburger.Internal.IR.DNF(DNFConjunct(..), DNFDisjunct, DNF, fromCompNF, quoteDNF) where


import           Cardano.ReCon.Common.Types (NatValue)
import           Cardano.ReCon.Integer.Polynomial.Term
import qualified Cardano.ReCon.Presburger.Formula as Rel
import           Cardano.ReCon.Presburger.Internal.IR.CompNF (CompNF)
import qualified Cardano.ReCon.Presburger.Internal.IR.CompNF as C
import           Cardano.ReCon.Presburger.Internal.IR.QuantifierFree (QuantifierFree)
import qualified Cardano.ReCon.Presburger.Internal.IR.QuantifierFree as Q

-- | A single conjunct in a DNF clause: either a strict less-than inequality
--   or a (possibly negated) divisibility atom.  All comparison relations have
--   already been reduced to @<@ (see
--   'Cardano.ReCon.Presburger.Internal.IR.CompNF.CompNF').
--
--   A 'DNF' is a disjunction (@[DNFDisjunct]@) of conjunctions
--   (@[DNFConjunct]@).  The empty disjunct list represents ⊥; a list
--   containing one empty conjunct represents ⊤.
data DNFConjunct =
     -- | i < i
     IntLt IntTerm IntTerm
     -- | ℕ⁺ \| i  (divisibility relation, first argument is a non-zero ℕ)
   | IntDiv NatValue IntTerm
     -- | ¬ (ℕ⁺ \| i)
   | IntNegDiv NatValue IntTerm deriving (Show, Eq, Ord)

type DNFDisjunct = [DNFConjunct]
type DNF = [DNFDisjunct]

fromCompNF :: CompNF -> DNF
fromCompNF (C.Or a b) = fromCompNF a ++ fromCompNF b
fromCompNF (C.And a b) = [p ++ q | p <- fromCompNF a, q <- fromCompNF b]
fromCompNF C.Top = [[]]
fromCompNF C.Bottom = []
fromCompNF (C.IntLt i j) = [[IntLt i j]]
fromCompNF (C.IntDiv n i) = [[IntDiv n i]]
fromCompNF (C.IntNegDiv n i) = [[IntNegDiv n i]]

-- | Embed a DNF back into QuantifierFree.
-- [] (empty disjunct list) = ⊥
-- [[]] (one empty conjunct) = ⊤
quoteDNF :: DNF -> QuantifierFree
quoteDNF []  = Q.Bottom
quoteDNF xss = foldr1 Q.Or (map quoteDisjunct xss)
  where
    quoteDisjunct [] = Q.Top
    quoteDisjunct cs = foldr1 Q.And (map quoteConjunct cs)
    quoteConjunct (IntLt i j)     = Q.IntBinRel Rel.Lt i j
    quoteConjunct (IntDiv h t)    = Q.IntDiv h t
    quoteConjunct (IntNegDiv h t) = Q.Not (Q.IntDiv h t)
