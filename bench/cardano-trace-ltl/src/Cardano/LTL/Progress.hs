{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE CPP #-}
module Cardano.LTL.Progress(next, terminate) where

import           Cardano.LTL.Lang.Formula
import qualified Cardano.LTL.Lang.Formula            as F
import           Cardano.LTL.Lang.HomogeneousFormula (HomogeneousFormula)
import qualified Cardano.LTL.Lang.HomogeneousFormula as H
import           Data.Map.Strict                     (lookup)
import qualified Data.Set                            as Set
import           Prelude                             hiding (lookup)
#ifdef CRITICAL_ERROR_ON_MISSING_KEY
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
  F.and $ flip fmap (Set.toList is) $ \(PropConstraint key t) ->
    case lookup key (props s c) of
      Just v  -> PropEq (Set.singleton (s, c)) t v
      Nothing ->
#ifdef CRITICAL_ERROR_ON_MISSING_KEY
        error $ "Missing key: " <> Text.unpack key
#else
        Bottom
#endif
next (Atom {}) _ = Bottom
next (PropForall x phi) s = PropForall x (next phi s)
next (PropForallN x dom phi) s = PropForallN x dom (next phi s)
next (PropEq rel a b) _ = PropEq rel a b

-- | This is an algorithm for (∅ ⊧ ◯ φ)
terminateNext :: Formula event a -> HomogeneousFormula event a
terminateNext (Next phi) = terminateNext phi
terminateNext (Forall _ phi)  = terminateNext phi
terminateNext (Or phi psi) = terminateNext phi `H.Or` terminateNext psi
terminateNext (And phi psi) = terminateNext phi `H.And` terminateNext psi
terminateNext (Implies phi psi) = terminateNext phi `H.Implies` terminateNext psi
terminateNext (Not phi) = H.Not (terminateNext phi)
terminateNext (PropForall x phi) = H.PropForall x (terminateNext phi)
terminateNext (PropForallN x dom phi) = H.PropForallN x dom (terminateNext phi)
terminateNext (PropEq rel t v) = H.PropEq rel t v
terminateNext (NextN _ phi) = terminateNext phi
terminateNext (Atom ty cs) = terminate (Atom ty cs)
terminateNext Bottom = terminate Bottom
terminateNext Top = terminate Top
terminateNext (ExistsN k phi) = terminateNext (unfoldExistsN k phi)
terminateNext (ForallN k phi) = terminateNext (unfoldForallN k phi)
terminateNext (UntilN k phi psi) = terminateNext (unfoldUntilN k phi psi)

-- | This is an algorithm for (∅ ⊧ φ)
terminate :: Formula event a -> HomogeneousFormula event a
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
terminate (PropForall x phi)      = H.PropForall x (terminate phi)
terminate (PropForallN x dom phi) = H.PropForallN x dom (terminate phi)
terminate (PropEq rel a b)        = H.PropEq rel a b
