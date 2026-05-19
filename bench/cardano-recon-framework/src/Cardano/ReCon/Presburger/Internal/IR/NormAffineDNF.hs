module Cardano.ReCon.Presburger.Internal.IR.NormAffineDNF
  ( NormAffineDNF
  , NormAffineDNFDisjunct
  , NormAffineDNFConjunct(..)
  , IntLtForm(..)
  , IntDivForm(..)
  , fromAffineDNF
  ) where
import           Cardano.ReCon.Common.Types (NatValue)
import           Cardano.ReCon.Integer.Polynomial.Term
import qualified Cardano.ReCon.Integer.Polynomial.Term as Term
import           Cardano.ReCon.Presburger.Internal.IR.AffineDNF (AffineDNF, AffineDNFConjunct,
                   AffineDNFDisjunct)
import qualified Cardano.ReCon.Presburger.Internal.IR.AffineDNF as Affine

import qualified Data.List as List

-- | A @<@-atom with the coefficient of @x@ normalised to 1.  Bounds take
--   the form @x < i@ ('LtL') or @i < x@ ('LtR'); constant atoms @i < j@
--   ('LtC') do not involve @x@ at all.
data IntLtForm =
    -- | x < i, where i ∈ ℤ[x̄\{x}]
    LtL IntTerm
    -- | i < x, where i ∈ ℤ⟦x̄\{x}⟧
  | LtR IntTerm
    -- i < j, where i, j ∈ ℤ⟦x̄\{x}⟧
  | LtC IntTerm IntTerm deriving (Show, Eq, Ord)

-- | A divisibility atom with the coefficient of @x@ normalised to 1.
data IntDivForm =
    -- | (h | x + i), where h ∈ ℕ⁺, i ∈ ℤ[x̄\{x}]
    DivL NatValue IntTerm
    -- | (h | i), where h ∈ ℕ⁺, i ∈ ℤ[x̄\{x}]
  | DivC NatValue IntTerm deriving (Show, Eq, Ord)

-- | A DNF conjunct in normalised affine form: the coefficient of the variable
--   @x@ being eliminated has been scaled to 1 in every atom.  This is the
--   canonical form required by Cooper's test-point enumeration step, where
--   bounds are read off directly as @x < i@ or @i < x@.
--   A 'NormAffineDNF' is a @[NormAffineDNFDisjunct]@ (disjunction) of
--   @[NormAffineDNFConjunct]@ (conjunction).
--   Produced from 'Cardano.ReCon.Presburger.Internal.IR.AffineDNF.AffineDNF' by 'fromAffineDNF'.
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

detIntLtForm :: Affine.IntLtForm -> NatValue
detIntLtForm (Affine.LtL k _) = k
detIntLtForm (Affine.LtR _ k) = k
detIntLtForm Affine.LtC{}     = 1

detIntDivForm :: Affine.IntDivForm -> NatValue
detIntDivForm (Affine.DivL _ k _) = k
detIntDivForm Affine.DivC{}       = 1

detConjunct :: AffineDNFConjunct -> NatValue
detConjunct (Affine.IntLt form) = detIntLtForm form
detConjunct (Affine.IntDiv form) = detIntDivForm form
detConjunct (Affine.IntNegDiv form) = detIntDivForm form

detDisjunct :: AffineDNFDisjunct -> NatValue
detDisjunct = List.foldl' (\acc c -> lcm acc (detConjunct c)) 1

normIntLtForm :: NatValue -> Affine.IntLtForm -> IntLtForm
normIntLtForm det (Affine.LtL k i) = LtL (Term.mul (fromIntegral (det `div` k)) i)
normIntLtForm det (Affine.LtR i k) = LtR (Term.mul (fromIntegral (det `div` k)) i)
normIntLtForm _ (Affine.LtC i j)   = LtC i j

normIntDivForm :: NatValue -> Affine.IntDivForm -> IntDivForm
normIntDivForm det (Affine.DivL h k i) = DivL (h * (det `div` k)) (Term.mul (fromIntegral (det `div` k)) i)
normIntDivForm _ (Affine.DivC h i) = DivC h i

normConjunct :: NatValue -> AffineDNFConjunct -> NormAffineDNFConjunct
normConjunct det (Affine.IntLt form) = IntLt (normIntLtForm det form)
normConjunct det (Affine.IntDiv form) = IntDiv (normIntDivForm det form)
normConjunct det (Affine.IntNegDiv form) = IntNegDiv (normIntDivForm det form)

normDisjunct :: NatValue -> AffineDNFDisjunct -> NormAffineDNFDisjunct
normDisjunct det c = IntDiv (DivL det (IntConst 0)) : map (normConjunct det) c

fromAffineDNF :: AffineDNF -> NormAffineDNF
fromAffineDNF = map (\d -> normDisjunct (detDisjunct d) d)
