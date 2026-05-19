{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.ReCon.Integer.Polynomial.Value (
    IntTermNF(..)
  , zero
  , plus
  , minus
  , mul
  , nullify
  , eval
  , quote
  , normalise
  ) where

import           Cardano.ReCon.Integer.Polynomial.Term (VariableIdentifier, IntTerm (..), IntValue)

import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | c + k₀ · x₀ + k₁ · x₁ + ... + kₙ · xₙ
data IntTermNF = IntTermNF {
  -- | Invariant: the coefficients must be non-zero.
  coeff    :: Map VariableIdentifier IntValue,
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
  IntTermNF{coeff = Map.filter (/= 0) $ Map.unionWith (-) coeff coeff', constant = constant - constant'}

-- | k * (c + k₀ · x₀ + k₁ · x₁ + ... + kₙ · xₙ)
--   =
--   ((k * c) + (k * k₀) · x₀ + (k * k₁) · x₁ + ... + (k * kₙ) · xₙ)
mul :: IntValue -> IntTermNF -> IntTermNF
mul k IntTermNF{..} = IntTermNF{constant = k * constant, coeff = Map.filter (/= 0) $ Map.map (* k) coeff}

setCoeff :: IntValue -> VariableIdentifier -> IntTermNF -> IntTermNF
setCoeff k x IntTermNF{..} = IntTermNF{coeff = Map.filter (/= 0) $ Map.insert x k coeff, constant}

nullify :: VariableIdentifier -> IntTermNF -> IntTermNF
nullify = setCoeff 0

eval :: IntTerm -> IntTermNF
eval (IntConst k)  = IntTermNF {coeff = Map.empty, constant = k}
eval (IntVar k x)  = IntTermNF {coeff = Map.filter (/= 0) (Map.singleton x k), constant = 0}
eval (IntSum a b)  = eval a `plus` eval b

quote :: IntTermNF -> IntTerm
quote IntTermNF{..} | null coeff = IntConst constant
quote IntTermNF{..} = IntSum (IntConst constant) vars where
  -- [] case is impossible here due to the clause above
  (x0, k0) : rest = Map.toList coeff
  vars            = List.foldl' go (IntVar k0 x0) rest
  go acc (x, k)   = IntSum acc (IntVar k x)

normalise :: IntTerm -> IntTerm
normalise = quote . eval
