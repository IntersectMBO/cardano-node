module Cardano.ReCon.Presburger.DNF(DNFConjunct(..), DNFDisjunct, DNF, fromCompNF) where


import           Cardano.ReCon.Presburger.CompNF (CompNF)
import qualified Cardano.ReCon.Presburger.CompNF as C
import           Cardano.ReCon.Presburger.IntTerm.Types

import           Data.Word (Word64)

data DNFConjunct =
     -- | i < i
     IntLt IntTerm IntTerm
     -- | ℕ⁺ \| i  (divisibility relation, first argument is a non-zero ℕ)
   | IntDiv Word64 IntTerm
     -- | ¬ (ℕ⁺ \| i)
   | IntNegDiv Word64 IntTerm deriving (Show, Eq, Ord)

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
