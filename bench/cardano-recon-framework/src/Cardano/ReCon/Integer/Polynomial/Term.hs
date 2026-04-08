module Cardano.ReCon.Integer.Polynomial.Term (
    VariableIdentifier
  , IntValue
  , IntTerm(..)
  , mul
  ) where

import           Cardano.ReCon.Common.Types (IntValue, VariableIdentifier)

-- | i, j ::= i + i | <int> · x | <int>, where <int> is an arbitrary integer
data IntTerm = IntVar IntValue VariableIdentifier | IntConst IntValue | IntSum IntTerm IntTerm deriving (Show, Eq, Ord)

-- k · i
-- k · (k' · x) = (k * k') · x
-- k · c = k * c
-- k · (i + j) = k · i + k · j
mul :: IntValue -> IntTerm -> IntTerm
mul k (IntVar k' x) = IntVar (k * k') x
mul k (IntConst k') = IntConst (k * k')
mul k (IntSum a b)  = IntSum (mul k a) (mul k b)
