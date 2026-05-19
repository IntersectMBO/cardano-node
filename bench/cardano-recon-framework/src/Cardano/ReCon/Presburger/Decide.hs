module Cardano.ReCon.Presburger.Decide (eval, quote) where

import           Cardano.ReCon.Presburger.Formula (Formula (Bottom, Top))
import           Cardano.ReCon.Presburger.Internal.CooperQE (eliminate)
import qualified Cardano.ReCon.Presburger.Internal.IR.ForallFree as F
import qualified Cardano.ReCon.Presburger.Internal.IR.QuantifierFree as Q

-- | Decide a closed Formula by eliminating all quantifiers and
-- evaluating the resulting ground QuantifierFree formula.
eval :: Formula -> Bool
eval = Q.eval . eliminate . F.fromFormula

-- | Embed a Bool into Formula.
quote :: Bool -> Formula
quote True  = Top
quote False = Bottom
