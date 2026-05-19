{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Cardano.ReCon.Presburger.Internal.IR.QuantifierFree
  ( QuantifierFree(..)
  , unfoldImplies
  , eval
  , quote
  ) where

import           Cardano.ReCon.Common.Types (NatValue)
import           Cardano.ReCon.Integer.Polynomial.Term
import           Cardano.ReCon.Presburger.Formula (BinRel (..))

-- | The quantifier-free fragment of Presburger arithmetic: propositional
--   connectives over atomic comparisons (@IntBinRel@) and divisibility
--   (@IntDiv@), with no quantifiers.
--   This is both the target form produced by Cooper's quantifier elimination
--   and the evaluation domain: 'eval' reduces a closed 'QuantifierFree'
--   sentence to a 'Bool'.
data QuantifierFree =
     -- | φ ∨ ψ
     Or QuantifierFree QuantifierFree
     -- | φ ∧ ψ
   | And QuantifierFree QuantifierFree
     -- | ¬ φ
   | Not QuantifierFree
     -- | φ ⇒ ψ
   | Implies QuantifierFree QuantifierFree
     -- | T
   | Top
     -- | ⊥
   | Bottom
     -- | i = i | i < i | i ≤ i | i > i | i ≥ i
   | IntBinRel BinRel IntTerm IntTerm
     -- | ℕ⁺ \| i  (divisibility relation, first argument is a non-zero ℕ)
   | IntDiv NatValue IntTerm deriving (Show, Eq, Ord)

unfoldImplies :: QuantifierFree -> QuantifierFree -> QuantifierFree
unfoldImplies a b = Or (Not a) b

-- | Evaluate a closed QuantifierFree formula to a Bool.
-- Precondition: no free variables (IntVar nodes must not appear in any IntTerm).
eval :: QuantifierFree -> Bool
eval (Or a b)          = eval a || eval b
eval (And a b)         = eval a && eval b
eval (Not a)           = not (eval a)
eval (Implies a b)     = not (eval a) || eval b
eval Top               = True
eval Bottom            = False
eval (IntBinRel r i j) = evalRel r (evalTerm i) (evalTerm j)
eval (IntDiv k t)      = evalTerm t `mod` fromIntegral k == 0

evalRel :: BinRel -> IntValue -> IntValue -> Bool
evalRel Eq  i j = i == j
evalRel Lt  i j = i <  j
evalRel Lte i j = i <= j
evalRel Gt  i j = i >  j
evalRel Gte i j = i >= j

-- | Evaluate a closed IntTerm to an integer value.
-- Precondition: no IntVar nodes.
evalTerm :: IntTerm -> IntValue
evalTerm (IntConst k)  = k
evalTerm (IntSum a b)  = evalTerm a + evalTerm b
evalTerm (IntVar _ x)  = error $ "evalTerm: free variable " <> show x

-- | Embed a Bool into QuantifierFree.
quote :: Bool -> QuantifierFree
quote True  = Top
quote False = Bottom
