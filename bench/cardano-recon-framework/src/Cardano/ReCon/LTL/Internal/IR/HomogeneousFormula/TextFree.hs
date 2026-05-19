module Cardano.ReCon.LTL.Internal.IR.HomogeneousFormula.TextFree (
    TextFree(..)
  , Extended(..)
  , eval
  ) where

import           Cardano.ReCon.Common.Types (BinRel (..))
import           Cardano.ReCon.Integer.Polynomial.Term (IntTerm)
import           Cardano.ReCon.LTL.Formula (VariableIdentifier)
import qualified Cardano.ReCon.Presburger.Decide as PD
import qualified Cardano.ReCon.Presburger.Formula as P

-- | An extended value: either a concrete value or a placeholder distinct from all concrete values.
data Extended a = Val a | Placeholder deriving (Show, Ord, Eq)

-- | `FinFree` with all text quantifiers and `PropTextEq` atoms eliminated.
--   Only integer operations remain.
data TextFree =
   ------------ Connective -------------
     Or TextFree TextFree
   | And TextFree TextFree
   | Not TextFree
   | Implies TextFree TextFree
   | Top
   | Bottom
   -------------------------------------

   ----------- Event property ----------
   | PropIntForall  VariableIdentifier TextFree
   | PropIntExists  VariableIdentifier TextFree
   | PropIntBinRel  BinRel IntTerm IntTerm
   deriving (Show, Eq, Ord)
   -------------------------------------

-- ---------------------------------------------------------------------------
-- Translation to Presburger arithmetic
-- ---------------------------------------------------------------------------

toPresburger :: TextFree -> P.Formula
toPresburger (Or phi psi)      = P.Or      (toPresburger phi) (toPresburger psi)
toPresburger (And phi psi)     = P.And     (toPresburger phi) (toPresburger psi)
toPresburger (Not phi)         = P.Not     (toPresburger phi)
toPresburger (Implies phi psi) = P.Implies (toPresburger phi) (toPresburger psi)
toPresburger Top               = P.Top
toPresburger Bottom            = P.Bottom
toPresburger (PropIntForall x phi)       = P.IntForall x (toPresburger phi)
toPresburger (PropIntExists x phi)       = P.IntExists x (toPresburger phi)
toPresburger (PropIntBinRel rel lhs rhs) = P.IntBinRel rel lhs rhs


-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

-- | Evaluate a `TextFree` formula by translating to Presburger arithmetic
--   and deciding via quantifier elimination.
eval :: TextFree -> Bool
eval = PD.eval . toPresburger
