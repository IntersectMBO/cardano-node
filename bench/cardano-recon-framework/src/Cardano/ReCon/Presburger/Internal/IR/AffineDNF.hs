module Cardano.ReCon.Presburger.Internal.IR.AffineDNF(IntLtForm(..), IntDivForm(..), AffineDNFConjunct(..), AffineDNFDisjunct, AffineDNF, fromDNF) where
import           Cardano.ReCon.Common.Types (NatValue)
import           Cardano.ReCon.Integer.Polynomial.Term
import           Cardano.ReCon.Integer.Polynomial.Value
import qualified Cardano.ReCon.Integer.Polynomial.Value as Value
import           Cardano.ReCon.Presburger.Internal.IR.DNF (DNF, DNFConjunct)
import qualified Cardano.ReCon.Presburger.Internal.IR.DNF as DNF

import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

-- | A @<@-atom in a DNF conjunct classified by how the variable @x@ being
--   eliminated appears in it.  Coefficients on @x@ may be non-unit positive
--   naturals.
data IntLtForm =
    -- | h · x < i, where h ∈ ℕ⁺, i ∈ ℤ[x̄\{x}]
    LtL NatValue IntTerm
    -- | i < h · x, where h ∈ ℕ⁺, i ∈ ℤ⟦x̄\{x}⟧
  | LtR IntTerm NatValue
    -- i < j, where i, j ∈ ℤ⟦x̄\{x}⟧
  | LtC IntTerm IntTerm deriving (Show, Eq, Ord)

-- | A divisibility atom classified by whether @x@ appears in it.
data IntDivForm =
    -- | (h | k · x + i), where h, k ∈ ℕ⁺, i ∈ ℤ[x̄\{x}]
    DivL NatValue NatValue IntTerm
    -- | (h | i), where h ∈ ℕ⁺, i ∈ ℤ[x̄\{x}]
  | DivC NatValue IntTerm deriving (Show, Eq, Ord)

-- | A DNF conjunct in affine form: each atom has been rewritten so that the
--   variable @x@ being eliminated appears at most once, with its coefficient
--   explicit.  An 'AffineDNF' is a @[AffineDNFDisjunct]@ (disjunction) of
--   @[AffineDNFConjunct]@ (conjunction).
--   Produced from 'Cardano.ReCon.Presburger.Internal.IR.DNF.DNF' by 'fromDNF'.
data AffineDNFConjunct =
     -- | h · x < i | i < h · x | i < j
     IntLt IntLtForm
     -- | (h | k · x + i) | (h | i)
   | IntDiv IntDivForm
     -- | ¬ (h | k · x + i) | ¬ (h | i)
   | IntNegDiv IntDivForm
   deriving (Show, Eq, Ord)

type AffineDNFDisjunct = [AffineDNFConjunct]

type AffineDNF = [AffineDNFDisjunct]

classifyIntLt :: VariableIdentifier -> IntTerm -> IntTerm -> IntLtForm
classifyIntLt x a b =
  let nfa = eval a in
  let nfb = eval b in
  let ka = fromMaybe 0 $ Map.lookup x nfa.coeff in
  let kb = fromMaybe 0 $ Map.lookup x nfb.coeff in
  case compare (ka - kb) 0 of
    -- (k(a) - k(b)) · x < ...
    GT -> LtL (fromIntegral $ ka - kb)
              (quote (nullify x nfb `minus` nullify x nfa))
    -- ... < ...
    EQ -> LtC (quote $ nullify x nfa) (quote $ nullify x nfb)
    -- ... < (k(b) - k(a)) · x
    LT -> LtR (quote (nullify x nfa `minus` nullify x nfb))
              (fromIntegral $ kb - ka)

classifyIntDiv :: VariableIdentifier -> NatValue -> IntTerm -> IntDivForm
classifyIntDiv x k t =
  let nft  = eval t
      coef = fromMaybe 0 $ Map.lookup x nft.coeff in
  if coef == 0
    then DivC k (normalise t)
    else let rest = nullify x nft
         in DivL k (fromIntegral $ abs coef)
                   (quote $ if coef > 0 then rest else Value.mul (-1) rest)

fromDNFConjunct :: VariableIdentifier -> DNFConjunct -> AffineDNFConjunct
fromDNFConjunct x (DNF.IntLt i j) = IntLt $ classifyIntLt x i j
fromDNFConjunct x (DNF.IntDiv k i) = IntDiv $ classifyIntDiv x k i
fromDNFConjunct x (DNF.IntNegDiv k i) = IntNegDiv $ classifyIntDiv x k i

fromDNF :: VariableIdentifier -> DNF -> AffineDNF
fromDNF x = map (map (fromDNFConjunct x))
