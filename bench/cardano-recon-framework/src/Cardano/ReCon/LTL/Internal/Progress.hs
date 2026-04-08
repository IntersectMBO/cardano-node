{-# LANGUAGE CPP #-}
module Cardano.ReCon.LTL.Internal.Progress(next, terminate) where

import           Cardano.ReCon.LTL.Formula
import qualified Cardano.ReCon.LTL.Formula as F
import           Cardano.ReCon.LTL.Internal.IR.HomogeneousFormula (HomogeneousFormula)
import qualified Cardano.ReCon.LTL.Internal.IR.HomogeneousFormula as H

import           Prelude hiding (lookup)

import           Data.Map.Strict (lookup)
import qualified Data.Set as Set

#ifdef CRASH_ON_MISSING_KEY
import qualified Data.Text                           as Text
#endif

-- This file concerns algorithmically checking formula satisfiability.
--  There are two parts:
--    *) (t t̄ ⊧ φ) for producing a new formula φ' such that the two are equi-satisfiable,
--          but the new one operates on the suffix of the original context: (t̄ ⊧ φ')
--    *) (∅ ⊧ φ) for checking satisfiability against the empty context.

-- | This is an algorithm for representing
--   (t t̄ ⊧ φ) in terms of ∃φ'. (t̄ ⊧ φ')
next :: (Event event ty, Eq ty) => Formula event ty -> event -> Formula event ty
next (Forall k phi) s = next (unfoldForall k phi) s
next (ForallN k phi) s = next (unfoldForallN k phi) s
next (ExistsN k phi) s = next (unfoldExistsN k phi) s
next (NextN k phi) s = next (unfoldNextN k phi) s
next (UntilN k phi psi) s = next (unfoldUntilN k phi psi) s
next (Next phi) _ = phi
next (And phi psi) s = And (next phi s) (next psi s)
next (Or phi psi) s = Or (next phi s) (next psi s)
next (Implies phi psi) s = Implies (next phi s) (next psi s)
next (Not phi) s = Not (next phi s)
next Bottom _ = Bottom
next Top _ = Top
next (Atom c is) s | ofTy s c =
  F.and $ flip fmap (Set.toList is) $ \case
    IntPropConstraint key t ->
      case lookup key (intProps s c) of
        Just v  -> PropIntBinRel Eq (Set.singleton (s, c)) t (IntConst v)
        Nothing ->
#ifdef CRASH_ON_MISSING_KEY
          error $ "Missing key: " <> Text.unpack key
#else
          Bottom
#endif
    TextPropConstraint key t ->
      case lookup key (textProps s c) of
        Just v  -> PropTextEq (Set.singleton (s, c)) t v
        Nothing ->
#ifdef CRASH_ON_MISSING_KEY
          error $ "Missing key: " <> Text.unpack key
#else
          Bottom
#endif
next (Atom {}) _ = Bottom
next (PropIntForall  x phi) s = PropIntForall  x (next phi s)
next (PropTextForall x phi) s = PropTextForall x (next phi s)
next (PropIntForallN  x dom phi) s = PropIntForallN  x dom (next phi s)
next (PropTextForallN x dom phi) s = PropTextForallN x dom (next phi s)
next (PropIntExists  x phi) s = PropIntExists  x (next phi s)
next (PropTextExists x phi) s = PropTextExists x (next phi s)
next (PropIntExistsN  x dom phi) s = PropIntExistsN  x dom (next phi s)
next (PropTextExistsN x dom phi) s = PropTextExistsN x dom (next phi s)
next (PropIntBinRel rel r a b) _ = PropIntBinRel rel r a b
next (PropTextEq rel a b)      _ = PropTextEq rel a b

-- | This is an algorithm for (∅ ⊧ ◯ φ)
terminateNext :: Formula event a -> HomogeneousFormula
terminateNext (Next phi) = terminateNext phi
terminateNext (Forall _ phi)  = terminateNext phi
terminateNext (Or phi psi) = terminateNext phi `H.Or` terminateNext psi
terminateNext (And phi psi) = terminateNext phi `H.And` terminateNext psi
terminateNext (Implies phi psi) = terminateNext phi `H.Implies` terminateNext psi
terminateNext (Not phi) = H.Not (terminateNext phi)
terminateNext (PropIntForall  x phi) = H.PropIntForall  x (terminateNext phi)
terminateNext (PropTextForall x phi) = H.PropTextForall x (terminateNext phi)
terminateNext (PropIntForallN  x dom phi) = H.PropIntForallN  x dom (terminateNext phi)
terminateNext (PropTextForallN x dom phi) = H.PropTextForallN x dom (terminateNext phi)
terminateNext (PropIntExists  x phi) = H.PropIntExists  x (terminateNext phi)
terminateNext (PropTextExists x phi) = H.PropTextExists x (terminateNext phi)
terminateNext (PropIntExistsN  x dom phi) = H.PropIntExistsN  x dom (terminateNext phi)
terminateNext (PropTextExistsN x dom phi) = H.PropTextExistsN x dom (terminateNext phi)
terminateNext (PropIntBinRel rel _ t v) = H.PropIntBinRel rel t v
terminateNext (PropTextEq _ t v)        = H.PropTextEq t v
terminateNext (NextN _ phi) = terminateNext phi
terminateNext (Atom ty cs) = terminate (Atom ty cs)
terminateNext Bottom = terminate Bottom
terminateNext Top = terminate Top
terminateNext (ExistsN k phi) = terminateNext (unfoldExistsN k phi)
terminateNext (ForallN k phi) = terminateNext (unfoldForallN k phi)
terminateNext (UntilN k phi psi) = terminateNext (unfoldUntilN k phi psi)

-- | This is an algorithm for (∅ ⊧ φ)
terminate :: Formula event a -> HomogeneousFormula
terminate (Forall k phi)          = terminate (unfoldForall k phi)
terminate (ForallN k phi)         = terminate (unfoldForallN k phi)
terminate (ExistsN k phi)         = terminate (unfoldExistsN k phi)
terminate (UntilN k phi psi)      = terminate (unfoldUntilN k phi psi)
terminate (NextN k phi)           = terminate (unfoldNextN k phi)
terminate (Atom _ _)              = H.Bottom
terminate (Next phi)              = terminateNext phi
terminate (And phi psi)           = H.And (terminate phi) (terminate psi)
terminate (Or phi psi)            = H.Or (terminate phi) (terminate psi)
terminate (Implies phi psi)       = H.Implies (terminate phi) (terminate psi)
terminate (Not phi)               = H.Not (terminate phi)
terminate Bottom                  = H.Bottom
terminate Top                     = H.Top
terminate (PropIntForall  x phi)       = H.PropIntForall  x (terminate phi)
terminate (PropTextForall x phi)       = H.PropTextForall x (terminate phi)
terminate (PropIntForallN  x dom phi)  = H.PropIntForallN  x dom (terminate phi)
terminate (PropTextForallN x dom phi)  = H.PropTextForallN x dom (terminate phi)
terminate (PropIntExists  x phi)       = H.PropIntExists  x (terminate phi)
terminate (PropTextExists x phi)       = H.PropTextExists x (terminate phi)
terminate (PropIntExistsN  x dom phi)  = H.PropIntExistsN  x dom (terminate phi)
terminate (PropTextExistsN x dom phi)  = H.PropTextExistsN x dom (terminate phi)
terminate (PropIntBinRel rel _ a b) = H.PropIntBinRel rel a b
terminate (PropTextEq _ a b)        = H.PropTextEq a b
