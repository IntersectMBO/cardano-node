module Cardano.ReCon.Presburger.IntTerm.Types(
    IntIdentifier
  , IntValue
  , NatValue
  , IntTerm(..)
  , IntTermNF(..)
  , mulTerm
  , zero
  , plus
  , minus
  , mul
  , nullify
  , nf
  , quote
  , normalise) where
import           Data.Int (Int64)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Word (Word64)

type IntIdentifier = Text
type IntValue = Int64
type NatValue = Word64

-- i, j ::= i + i | <int> · x | <int>, where <int> is an arbitrary integer
data IntTerm = IntVar IntValue IntIdentifier | IntConst Int64 | IntSum IntTerm IntTerm deriving (Show, Eq, Ord)

-- k · i
-- k · (k' · x) = (k * k') · x
-- k · c = k * c
-- k · (i + j) = k · i + k · j
mulTerm :: IntValue -> IntTerm -> IntTerm
mulTerm k (IntVar k' x) = IntVar (k * k') x
mulTerm k (IntConst k') = IntConst (k * k')
mulTerm k (IntSum a b) = IntSum (mulTerm k a) (mulTerm k b)

-- | c + k₀ · x₀ + k₁ · x₁ + ... + kₙ · xₙ
data IntTermNF = IntTermNF {
  -- | Invariant: the coefficients must be non-zero.
  coeff :: Map IntIdentifier IntValue,
  constant :: IntValue
} deriving (Show, Eq, Ord)

-- 0 + 0 · x₀ + ... + 0 · xₙ
zero :: IntTermNF
zero = IntTermNF{coeff = Map.empty, constant = 0}

-- | (c + k₀ · x₀ + k₁ · x₁ + ... + kₙ · xₙ) + (c' + k₀' · x₀ + k₁' · x₁ + ... + kₙ' · xₙ)
--   =
--   ((c + c') + (k₀ + k₀') · x₀ + (k₁ + k₁') · x₁ + ... + (kₙ + kₙ') · xₙ)
plus :: IntTermNF -> IntTermNF -> IntTermNF
plus IntTermNF{coeff, constant} IntTermNF{coeff = coeff', constant = constant'} =
  IntTermNF{coeff = Map.filter (/= 0) $ Map.unionWith (+) coeff coeff', constant = constant + constant'}

minus :: IntTermNF -> IntTermNF -> IntTermNF
minus IntTermNF{coeff, constant} IntTermNF{coeff = coeff', constant = constant'} =
  IntTermNF{coeff = Map.filter (/= 0) $ Map.unionWith (-) coeff coeff', constant = constant + constant'}

-- | k * (c + k₀ · x₀ + k₁ · x₁ + ... + kₙ · xₙ)
--   =
--   ((k * c) + (k * k₀) · x₀ + (k * k₁) · x₁ + ... + (k * kₙ) · xₙ)
mul :: IntValue -> IntTermNF -> IntTermNF
mul k IntTermNF{..} = IntTermNF{constant = k * constant, coeff = Map.filter (/= 0) $ Map.map (* k) coeff}

setCoeff :: IntValue -> IntIdentifier -> IntTermNF -> IntTermNF
setCoeff k x IntTermNF{..} = IntTermNF{coeff = Map.filter (/= 0) $ Map.insert x k coeff, constant}

nullify :: IntIdentifier -> IntTermNF -> IntTermNF
nullify = setCoeff 0

nf :: IntTerm -> IntTermNF
nf (IntConst k) = IntTermNF {coeff = Map.empty, constant = k}
nf (IntVar k x) = IntTermNF {coeff = Map.filter (/= 0) (Map.singleton x k), constant = 0}
nf (IntSum a b) = nf a `plus` nf b

quote :: IntTermNF -> IntTerm
quote IntTermNF{..} = IntSum (IntConst constant) (List.foldl' go (IntConst 0) (Map.toList coeff)) where
  go :: IntTerm -> (IntIdentifier, IntValue) -> IntTerm
  go lhs (x, k) = IntSum lhs (IntVar k x)

normalise :: IntTerm -> IntTerm
normalise = quote . nf
