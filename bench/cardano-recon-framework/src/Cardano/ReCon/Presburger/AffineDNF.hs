module Cardano.ReCon.Presburger.AffineDNF(IntLtForm(..), IntDivForm(..), AffineDNFConjunct(..), AffineDNFDisjunct, AffineDNF, fromDNF) where
import           Cardano.ReCon.Presburger.DNF (DNF, DNFConjunct)
import           qualified Cardano.ReCon.Presburger.DNF as DNF
import           Cardano.ReCon.Presburger.IntTerm.Types

import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)

data IntLtForm =
    -- | h · x < i, where h ∈ ℕ⁺, i ∈ ℤ[x̄\{x}]
    LtL Word64 IntTerm
    -- | i < h · x, where h ∈ ℕ⁺, i ∈ ℤ⟦x̄\{x}⟧
  | LtR IntTerm Word64
    -- i < j, where i, j ∈ ℤ⟦x̄\{x}⟧
  | LtC IntTerm IntTerm deriving (Show, Eq, Ord)

data IntDivForm =
    -- | (h | k · x + i), where h, k ∈ ℕ⁺, i ∈ ℤ[x̄\{x}]
    DivL Word64 Word64 IntTerm
    -- | (h | i), where h ∈ ℕ⁺, i ∈ ℤ[x̄\{x}]
  | DivC Word64 IntTerm deriving (Show, Eq, Ord)

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

classifyIntLt :: IntIdentifier -> IntTerm -> IntTerm -> IntLtForm
classifyIntLt x a b =
  let nfa = nf a in
  let nfb = nf b in
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

classifyIntDiv :: IntIdentifier -> Word64 -> IntTerm -> IntDivForm
classifyIntDiv x k t =
  let coef = fromMaybe 0 $ Map.lookup x (nf t).coeff in
  if coef == 0
    then DivC k (normalise t)
    else DivL k (fromIntegral $ abs coef) (quote $ nullify x (nf t))

fromDNFConjunct :: IntIdentifier -> DNFConjunct -> AffineDNFConjunct
fromDNFConjunct x (DNF.IntLt i j) = IntLt $ classifyIntLt x i j
fromDNFConjunct x (DNF.IntDiv k i) = IntDiv $ classifyIntDiv x k i
fromDNFConjunct x (DNF.IntNegDiv k i) = IntNegDiv $ classifyIntDiv x k i

fromDNF :: IntIdentifier -> DNF -> AffineDNF
fromDNF x = map (map (fromDNFConjunct x))
