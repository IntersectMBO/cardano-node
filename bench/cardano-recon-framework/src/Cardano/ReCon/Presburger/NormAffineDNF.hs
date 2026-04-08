module Cardano.ReCon.Presburger.NormAffineDNF(NormAffineDNF, fromAffineDNF) where
import           Cardano.ReCon.Presburger.AffineDNF (AffineDNFConjunct, AffineDNFDisjunct, AffineDNF)
import qualified Cardano.ReCon.Presburger.AffineDNF as Affine
import           Cardano.ReCon.Presburger.IntTerm.Types

import qualified Data.List as List
import           Data.Word (Word64)

data IntLtForm =
    -- | x < i, where i ∈ ℤ[x̄\{x}]
    LtL IntTerm
    -- | i < x, where i ∈ ℤ⟦x̄\{x}⟧
  | LtR IntTerm
    -- i < j, where i, j ∈ ℤ⟦x̄\{x}⟧
  | LtC IntTerm IntTerm deriving (Show, Eq, Ord)

data IntDivForm =
    -- | (h | x + i), where h ∈ ℕ⁺, i ∈ ℤ[x̄\{x}]
    DivL Word64 IntTerm
    -- | (h | i), where h ∈ ℕ⁺, i ∈ ℤ[x̄\{x}]
  | DivC Word64 IntTerm deriving (Show, Eq, Ord)

data NormAffineDNFConjunct =
     -- | x < i | i < x | i < j
     IntLt IntLtForm
     -- | (h | x + i) | (h | i)
   | IntDiv IntDivForm
     -- | ¬ (h | x + i) | ¬ (h | i)
   | IntNegDiv IntDivForm
   deriving (Show, Eq, Ord)

type NormAffineDNFDisjunct = [NormAffineDNFConjunct]

type NormAffineDNF = [NormAffineDNFDisjunct]

-- x̄ |- ∃x. 2 · x < i ∧ j < k ∧ p < 3 · x
-- <=>
-- x̄ |- ∃x. 6 · x < 3 · i ∧ j < k ∧ 2 · p < 6 · x
-- x̄ |- ∃x. 6|x ∧ x < 3 · i ∧ j < k ∧ 2 · p < x

detIntLtForm :: Affine.IntLtForm -> Word64
detIntLtForm (Affine.LtL k _) = k
detIntLtForm (Affine.LtR _ k) = k
detIntLtForm Affine.LtC{}     = 1

detIntDivForm :: Affine.IntDivForm -> Word64
detIntDivForm (Affine.DivL _ k _) = k
detIntDivForm Affine.DivC{}       = 1

detConjunct :: AffineDNFConjunct -> Word64
detConjunct (Affine.IntLt form) = detIntLtForm form
detConjunct (Affine.IntDiv form) = detIntDivForm form
detConjunct (Affine.IntNegDiv form) = detIntDivForm form

detDisjunct :: AffineDNFDisjunct -> Word64
detDisjunct = List.foldl' (\acc c -> lcm acc (detConjunct c)) 1

normIntLtForm :: Word64 -> Affine.IntLtForm -> IntLtForm
normIntLtForm det (Affine.LtL k i) = LtL (mulTerm (fromIntegral (det `div` k)) i)
normIntLtForm det (Affine.LtR i k) = LtR (mulTerm (fromIntegral (det `div` k)) i)
normIntLtForm _ (Affine.LtC i j)   = LtC i j

normIntDivForm :: Word64 -> Affine.IntDivForm -> IntDivForm
normIntDivForm det (Affine.DivL h k i) = DivL (h * (det `div` k)) (mulTerm (fromIntegral (det `div` k)) i)
normIntDivForm _ (Affine.DivC h i) = DivC h i

normConjunct :: Word64 -> AffineDNFConjunct -> NormAffineDNFConjunct
normConjunct det (Affine.IntLt form) = IntLt (normIntLtForm det form)
normConjunct det (Affine.IntDiv form) = IntDiv (normIntDivForm det form)
normConjunct det (Affine.IntNegDiv form) = IntNegDiv (normIntDivForm det form)

normDisjunct :: Word64 -> AffineDNFDisjunct -> NormAffineDNFDisjunct
normDisjunct det c = IntDiv (DivL det (IntConst 0)) : map (normConjunct det) c

fromAffineDNF :: AffineDNF -> NormAffineDNF
fromAffineDNF = map (\d -> normDisjunct (detDisjunct d) d)
