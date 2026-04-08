module Cardano.ReCon.Presburger.Internal.CooperQE (eliminate, eliminateExists) where

import           Cardano.ReCon.Common.Types (NatValue)
import           Cardano.ReCon.Integer.Polynomial.Term
import qualified Cardano.ReCon.Presburger.Internal.IR.AffineDNF as AffineDNF
import qualified Cardano.ReCon.Presburger.Internal.IR.CompNF as CompNF
import           Cardano.ReCon.Presburger.Internal.IR.DNF (DNF, DNFConjunct (..), quoteDNF)
import qualified Cardano.ReCon.Presburger.Internal.IR.DNF as DNF
import qualified Cardano.ReCon.Presburger.Internal.IR.ForallFree as F
import qualified Cardano.ReCon.Presburger.Internal.IR.NegNF as NegNF
import qualified Cardano.ReCon.Presburger.Internal.IR.NormAffineDNF as NormAffineDNF
import           Cardano.ReCon.Presburger.Internal.IR.QuantifierFree (QuantifierFree)
import qualified Cardano.ReCon.Presburger.Internal.IR.QuantifierFree as Q

import           Prelude hiding (Foldable (..))

import           Data.List (foldl', foldr, null)

-- | Eliminate all existential quantifiers from a ForallFree formula,
-- producing an equivalent QuantifierFree formula.
-- Proceeds bottom-up: inner quantifiers are eliminated before outer ones.
eliminate :: F.ForallFree -> QuantifierFree
eliminate (F.Or a b)          = Q.Or (eliminate a) (eliminate b)
eliminate (F.And a b)         = Q.And (eliminate a) (eliminate b)
eliminate (F.Not a)           = Q.Not (eliminate a)
eliminate (F.Implies a b)     = Q.Implies (eliminate a) (eliminate b)
eliminate F.Top               = Q.Top
eliminate F.Bottom            = Q.Bottom
eliminate (F.IntBinRel r i j) = Q.IntBinRel r i j
eliminate (F.IntDiv k t)      = Q.IntDiv k t
eliminate (F.IntExists x body) =
  -- Recursively eliminate inner quantifiers first, yielding a
  -- quantifier-free body, then apply the full Cooper pipeline.
  eliminateLeaf x (eliminate body)

-- | Run the full Cooper pipeline on ∃x. qf and return a QuantifierFree result.
eliminateLeaf :: VariableIdentifier -> QuantifierFree -> QuantifierFree
eliminateLeaf x =
    quoteDNF
  . eliminateExists
  . NormAffineDNF.fromAffineDNF
  . AffineDNF.fromDNF x
  . DNF.fromCompNF
  . CompNF.fromNegNF
  . NegNF.fromQuantifierFree

-- | Eliminate ∃x from a formula already in NormAffineDNF form w.r.t. x.
-- Returns a quantifier-free DNF in the remaining variables.
--
-- Algorithm (Cooper's theorem, unified form):
--
--   ∃x. C(x)  ⟺  ∨_{j=1}^{δ} C₋∞(j)  ∨  ∨_{lᵢ ∈ LB} ∨_{j=1}^{δ} C(lᵢ + j)
--
-- where δ = lcm of all divisors in divisibility constraints involving x,
-- and C₋∞ is C with all upper-bound atoms dropped.
eliminateExists :: NormAffineDNF.NormAffineDNF -> DNF
eliminateExists = concatMap elimDisjunct

-- ── per-disjunct ─────────────────────────────────────────────────────────────

data Partitioned = Partitioned
  { lbs     :: [IntTerm]           -- lᵢ from  lᵢ < x
  , ubs     :: [IntTerm]           -- uⱼ from  x < uⱼ
  , divs    :: [(NatValue, IntTerm)] -- (h, e) from  h | x + e
  , negDivs :: [(NatValue, IntTerm)] -- (h, e) from  ¬(h | x + e)
  , frees   :: [DNFConjunct]       -- x-free atoms
  }

emptyPartitioned :: Partitioned
emptyPartitioned = Partitioned [] [] [] [] []

classify :: NormAffineDNF.NormAffineDNFConjunct -> Partitioned -> Partitioned
classify (NormAffineDNF.IntLt (NormAffineDNF.LtR l))        p = p { lbs     = l : lbs p }
classify (NormAffineDNF.IntLt (NormAffineDNF.LtL u))        p = p { ubs     = u : ubs p }
classify (NormAffineDNF.IntLt (NormAffineDNF.LtC i j))      p = p { frees   = DNF.IntLt i j    : frees p }
classify (NormAffineDNF.IntDiv (NormAffineDNF.DivL h e))    p = p { divs    = (h, e)            : divs p }
classify (NormAffineDNF.IntDiv (NormAffineDNF.DivC h i))    p = p { frees   = DNF.IntDiv h i    : frees p }
classify (NormAffineDNF.IntNegDiv (NormAffineDNF.DivL h e)) p = p { negDivs = (h, e)            : negDivs p }
classify (NormAffineDNF.IntNegDiv (NormAffineDNF.DivC h i)) p = p { frees   = DNF.IntNegDiv h i : frees p }

elimDisjunct :: NormAffineDNF.NormAffineDNFDisjunct -> DNF
elimDisjunct conjuncts =
  let p     = foldr classify emptyPartitioned conjuncts
      delta = foldl' (\acc (d, _) -> lcm acc d) 1 (divs p)
      -- -∞ test set: only applicable when there are no lower bounds.
      -- When lower bounds exist every substitution x = j makes each atom
      -- l < j potentially true, but at x → -∞ every l < x is FALSE, so
      -- those conjuncts are all False and contribute nothing.
      infBranch = [ substituteAt True  p (IntConst (fromIntegral j))
                  | null (lbs p)
                  , j <- [1 .. delta] ]
      -- lower-bound test set: x = lb + j
      lbBranch  = [ substituteAt False p (IntSum lb (IntConst (fromIntegral j)))
                  | lb <- lbs p
                  , j  <- [1 .. delta] ]
  in infBranch ++ lbBranch

-- | Build one DNFDisjunct by substituting x = t into all conjuncts.
-- When isInf is True the upper-bound atoms (x < uⱼ) are dropped.
substituteAt :: Bool -> Partitioned -> IntTerm -> [DNFConjunct]
substituteAt isInf p t =
  frees p
  ++ (if isInf then [] else [DNF.IntLt t u       | u      <- ubs     p])
  ++ [DNF.IntLt l t                              | l      <- lbs     p]
  ++ [DNF.IntDiv    h (IntSum t e)               | (h, e) <- divs    p]
  ++ [DNF.IntNegDiv h (IntSum t e)               | (h, e) <- negDivs p]
